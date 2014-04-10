-- | Simplified interface for go.  Defines the standard black/white player,
-- and re-exports versions of common functions which take 2D co-ordinates
-- for the board position rather than a Repa co-ordinate.  This allows you
-- to use the Go package for standard 2D, 2-player games without having to
-- pull in the Repa dependency.

module Game.Go.Simple
  ( StandardPlayer (..)
  , Board
  , Game
  , Rules
  -- * Board
  , empty
  , allPoints
  , set
  , at
  , relative
  , neighbours
  , within
  , surrounding
  , matchingNeighbours
  , chain
  , liberties
  , territory
  , territories
  , pressuredGroups
  , placeStone
  -- * Game
  , initGame
  , initGameWithBoard
  , setPlayer
  , playMove
  , playPass
  , isLegal
  -- * Go game DSL
  , go
  , black
  , white
  , setBlack
  , setWhite
  ) where

import qualified Data.Array.Repa as R
import Data.Array.Repa           (Z (..), (:.) (..))

import Game.Go.Monad

import qualified Game.Go.Board   as B
import Game.Go.Board             (Player, Territory(..))

import qualified Game.Go.Game    as G
import Game.Go.Game              (Illegal(..))

import Control.Arrow

-- | Standard go player.  Black or White; Black plays first.
data StandardPlayer = Black | White deriving (Eq, Enum, Bounded, Ord)
instance B.Player StandardPlayer

-- | 2-dimensional game board.
type Board p = B.Board R.DIM2 p

-- | Simplified 'Game.Go.Game' type for 2-dimensional games.
type Game p = G.Game R.DIM2 p

-- | Simplified 'Game.Go.Rules' type for 2-dimensional rulesets.
type Rules p = G.Rules R.DIM2 p

-- | Create an empty `Board` with size w, h
-- The player is parameterized in the return type; to create an empty
-- board at the ghci prompt, for example, you must do something like the
-- following:
--
-- > empty 19 19 :: Board StandardPlayer
empty :: Player p => Int -> Int -> Board p
empty w h = B.empty $ tupleToRepaCoords (w, h)

-- | Get a list of all valid co-ordinates on the given `Board`.
allPoints :: Board p -> [(Int, Int)]
allPoints = map repaCoordsToTuple . B.allPoints

-- | Set the value of a list of co-ordinates.
set :: Board p -> [(Int, Int)] -> Maybe p -> Board p
set b coords = B.set b (map tupleToRepaCoords coords)

-- | Get the value at a certain co-ordinate.
at :: Board p -> (Int, Int) -> Maybe p
at b = B.at b . tupleToRepaCoords

-- | Return a list of co-ordinates offset by @Offset@.
relative :: [(Int, Int)] -- ^ Initial co-ordinates
         -> (Int, Int)   -- ^ Offset
         -> [(Int, Int)] -- ^ Result
relative coords = map repaCoordsToTuple
                . B.relative (map tupleToRepaCoords coords)
                . tupleToRepaCoords

-- | Return a list of co-ordinates surrounding the given co-ordinate.
neighbours :: (Int, Int) -> [(Int, Int)]
neighbours = map repaCoordsToTuple . B.neighbours . tupleToRepaCoords

-- | Filters the supplied co-ordinates, returning only those which lie
-- within the supplied `Board`.
within :: Board p -> [(Int, Int)] -> [(Int, Int)]
within b = map repaCoordsToTuple . B.within b . map tupleToRepaCoords

-- | Return a list of co-ordinates surrounding the supplied co-ordinates.
-- Co-ordinates that lie outside the boundaries of the passed `Board` are
-- filtered out.
surrounding :: Board p -> [(Int, Int)] -> [(Int, Int)]
surrounding b = map repaCoordsToTuple . B.surrounding b . map tupleToRepaCoords

-- | Query the Board to find which of the stones surrounding a list of coords
--   match the value of point.
matchingNeighbours :: Player p
                   => Board p -> Maybe p -> [(Int, Int)] -> [(Int, Int)]
matchingNeighbours b v = map repaCoordsToTuple
                       . B.matchingNeighbours b v
                       . map tupleToRepaCoords

-- | Return the co-ordinates of all points in the chain of which the
-- supplied point is a member.  A point is a member of a chain if there
-- is a direct line, horizontally or vertically, leading to another point
-- of the same colour.
-- Requesting the chain for an Empty point returns the contiguous range
-- of empty points.
chain :: Player p => Board p -> (Int, Int) -> [(Int, Int)]
chain b = map repaCoordsToTuple . B.chain b . tupleToRepaCoords

-- | Calculate the liberties from a chain of points.
liberties :: Player p  => Board p -> [(Int, Int)] -> [(Int, Int)]
liberties b = map repaCoordsToTuple . B.liberties b . map tupleToRepaCoords

-- | Return the territorial status of the passed position, alongside the
-- chain of which it is a member.
territory :: Player p => Board p -> (Int, Int) -> (Territory p, [(Int, Int)])
territory b = second (map repaCoordsToTuple) . B.territory b . tupleToRepaCoords

-- | Return a list of all chains and empty areas as returned by `territory`.
territories :: Player p => Board p -> [(Territory p, [[(Int, Int)]])]
territories = map (second $ map (map repaCoordsToTuple)) . B.territories

-- | Returns a list of groups of stones adjacent to a point @x@ whose colour
-- differs from that of player @p@.
pressuredGroups :: Player p
                => Board p -- ^ The current state of the board
                -> p              -- ^ The player @p@ making the threat
                -> (Int, Int)     -- ^ The position @x@ where @p@ will play
                -> [[(Int, Int)]] -- ^ List of pressured groups
pressuredGroups b p = map (map repaCoordsToTuple) . B.pressuredGroups b p . tupleToRepaCoords

-- | Returns the state of the board after placing a stone of colour @p@ in
-- position @x@, along with a list of stones taken as a result.  Does not
-- check the legality of the move.
placeStone :: Player p
           => Board p                      -- ^ The current state of the board
           -> p                            -- ^ The player, @p@
           -> (Int, Int)                   -- ^ The position, @x@
           -> ([(p, (Int, Int))], Board p) -- ^ List of taken stones and the
                                           -- resulting board position
placeStone b p = first (map $ second repaCoordsToTuple) . B.placeStone b p . tupleToRepaCoords

-- | Initialises a new game with the given board dimensions.
initGame :: Player p => Rules p -> (Int, Int) -> Game p
initGame r = G.initGame r . tupleToRepaCoords

-- | Initialises a new game with a starting board (useful for handicap games).
initGameWithBoard :: Player p => Rules p -> Board p -> Game p
initGameWithBoard = G.initGameWithBoard

-- | Set the current player manually.
setPlayer :: Player p => Game p -> p -> Game p
setPlayer = G.setPlayer

-- | Play a move.  Checks the legality of the move, and if it is legal,
-- returns the game state following the move.
playMove :: Player p
         => Game p -> p -> (Int, Int) -> Either Illegal (Game p)
playMove g p = G.playMove g p . tupleToRepaCoords

-- | Play a pass.  The number of passes will be incremented.  The game is
-- over when all players have passed sequentially, and then the first
-- player to pass passes again.
playPass :: Player p => Game p -> p -> Either Illegal (Game p)
playPass = G.playPass

-- | Check the legality of a move.
isLegal :: Player p => Game p -> p -> (Int, Int) -> Bool
isLegal g p = G.isLegal g p . tupleToRepaCoords

-- | Run the given 'Go' monad on a standard 19x19 board.
go :: (Player p) => Rules p -> Go R.DIM2 p a -> (a, Game p)
go r = goWithSize r (Z :. 19 :. 19)

-- | Set the supplied co-ordinates to Black.
setBlack :: [(Int, Int)] -> Go R.DIM2 StandardPlayer ()
setBlack = flip setM (Just Black) . map tupleToRepaCoords

-- | Set the supplied co-ordinates to White.
setWhite :: [(Int, Int)] -> Go R.DIM2 StandardPlayer ()
setWhite = flip setM (Just White) . map tupleToRepaCoords

-- | Play a move as Black.
black :: Int -> Int -> Go R.DIM2 StandardPlayer (Either Illegal ())
black x y = playMoveM Black (Z :. x :. y)

-- | Play a move as White.
white :: Int -> Int -> Go R.DIM2 StandardPlayer (Either Illegal ())
white x y = playMoveM White (Z :. x :. y)

-- Conversion helpers
tupleToRepaCoords :: (Int, Int) -> R.DIM2
tupleToRepaCoords (x, y) = Z :. x :. y

repaCoordsToTuple :: R.DIM2 -> (Int, Int)
repaCoordsToTuple (Z :. x :. y) = (x, y)
