-- | Defines the game board and queries that can be made on that board.

module Game.Go.Board 
  ( -- * Datatypes
    Board     (..)
  , Player
  , Territory (..)

    -- * Initialization and access
  , empty
  , allPoints
  , set
  , at

    -- * Utilities
    -- | These functions allow us to manipulate co-ordinates without
    -- requiring any `Board` state, and are useful for building other
    -- queries.
  , relative
  , neighbours

    -- * Queries and analysis
  , within
  , surrounding
  , matchingNeighbours
  , chain
  , liberties
  , territory
  , territories
  , pressuredGroups
  , placeStone
  ) where

import qualified Data.Vector         as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Map.Strict     as M
import qualified Data.Array.Repa     as R

import Data.Array.Repa               ((!))
import Data.Array.Repa.Repr.Vector

import Data.List                     ((\\), nub, union)
import Data.Maybe

import Control.Monad

-- | The board is represented as a Repa `Data.Array.Repa.Array` parameterized
-- in its shape and the `Player`s playing on the board.  This allows for games
-- of any size and dimensionality, with any number of players.
newtype Board sh p = Board { asArray :: R.Array V sh (Maybe p) } deriving (Show, Eq)

-- | The `Player` class is a convenience describing the various properties
-- a player datatype must conform to.  Anything implementing the
-- typeclasses mentioned here can be made a `Player`, for example:
--
-- > data StandardPlayer = Black | White deriving (Enum, Eq, Ord, Bounded)
-- > instance Player StandardPlayer
class (Enum p, Eq p, Ord p, Bounded p) => Player p

-- | Territory may be defined by the empty points surrounded by stones of
-- a certain colour, or by that and the surrounding stones themselves.  The
-- `Territory` type allows for both, by defining stones as either
-- `SurroundedBy` or `OccupiedBy` a particular player.
--
-- Empty points surrounded by stones of various colours are defined as
-- `Undecided`.
data Territory p = Undecided
                 | SurroundedBy p
                 | OccupiedBy p
                 deriving (Eq, Ord, Show)

-- | Create an empty `Board` with size and dimensionality described by
-- @sh@.  The player is parameterized in the return type; to create an empty
-- board at the ghci prompt, for example, you must do something like the
-- following:
--
-- > empty (Z :. 19 :. 19) :: Board R.DIM2 StandardPlayer
empty :: (R.Shape sh, Player p) => sh -> Board sh p
empty sh = Board $ fromListVector sh $ replicate (R.size sh) Nothing

-- | Get a list of all valid co-ordinates on the given `Board`.
allPoints :: R.Shape sh => Board sh p -> [sh]
allPoints = R.toList . flip R.fromFunction id . R.extent . asArray

-- | Set the value of a list of co-ordinates.
set :: R.Shape sh => Board sh p -> [sh] -> Maybe p -> Board sh p
set b coords v = Board $ asArray b // zip coords (repeat v)

(//) :: R.Shape sh => R.Array V sh a -> [(sh,a)] -> R.Array V sh a
(//) arr us = fromVector sh . V.modify f $ toVector arr
  where sh   = R.extent arr
        f mv = forM_ us $ \(k,x) -> MV.write mv (R.toIndex sh k) x

-- | Get the value at a certain co-ordinate.
at :: R.Shape sh => Board sh p -> sh -> Maybe p
at b = (asArray b !)

-- | Return a list of co-ordinates offset by @Offset@.
relative :: R.Shape sh
         => [sh] -- ^ Initial co-ordinates
         -> sh   -- ^ Offset
         -> [sh] -- ^ Result
relative coords = zipWith R.addDim coords . repeat

-- | Return a list of co-ordinates surrounding the given co-ordinate.
neighbours :: R.Shape sh => sh -> [sh]
neighbours coord = relative (concatMap axisNeighbours axes) coord
  where
    axisNeighbours a = map (R.shapeOfList . axisLength a) [1, -1]
    axisLength a l   = map (setDim a l) axes
    setDim a v i     = if a == i then v else 0
    axes             = [1..R.rank coord]

-- | Filters the supplied co-ordinates, returning only those which lie
-- within the supplied `Board`.
within :: R.Shape sh => Board sh p -> [sh] -> [sh]
within = filter . R.inShape . R.extent . asArray

-- | Return a list of co-ordinates surrounding the supplied co-ordinates.
-- Co-ordinates that lie outside the boundaries of the passed `Board` are
-- filtered out.
surrounding :: R.Shape sh => Board sh p -> [sh] -> [sh]
surrounding b xs = within b $ foldr (union . neighbours) [] xs \\ xs

-- | Query the Board to find which of the stones surrounding a list of coords
--   match the value of point.
matchingNeighbours :: (R.Shape sh, Player p)
                   => Board sh p -> Maybe p -> [sh] -> [sh]
matchingNeighbours b v = filter matches . surrounding b
  where matches x = asArray b ! x == v

-- | Return the co-ordinates of all points in the chain of which the
-- supplied point is a member.  A point is a member of a chain if there
-- is a direct line, horizontally or vertically, leading to another point
-- of the same colour.
-- Requesting the chain for an Empty point returns the contiguous range
-- of empty points.
chain :: (R.Shape sh, Player p) => Board sh p -> sh -> [sh]
chain b x = accumulate [] [x] $ asArray b ! x
  where accumulate xs [] _ = xs
        accumulate xs ys v = accumulate xs' ys' v
          where xs' = xs `union` ys
                ys' = matchingNeighbours b v xs' \\ xs'

-- | Calculate the liberties from a chain of points.
liberties :: (R.Shape sh, Player p)  => Board sh p -> [sh] -> [sh]
liberties b = filter isEmpty . surrounding b
  where isEmpty = isNothing . (asArray b !)

-- | Return the territorial status of the passed position, alongside the
-- chain of which it is a member.
territory :: (R.Shape sh, Player p) => Board sh p -> sh -> (Territory p, [sh])
territory b x = (statusFor x, ch)
  where statusFor = maybe (emptyStatus b ch) OccupiedBy . (asArray b !)
        ch        = chain b x

-- | Return a list of all chains and empty areas as returned by `territory`.
territories :: (R.Shape sh, Player p) => Board sh p -> [(Territory p, [[sh]])]
territories b = explore (allPoints b) M.empty
  where explore []     acc = M.toList acc
        explore (x:xs) acc = explore (xs \\ ch) (M.alter addChain s acc)
          where (s, ch)  = territory b x
                addChain = Just . maybe [ch] (ch:)

-- Returns the status of a chain of empty points.  This is an internal
-- function which makes a lot of assumptions about its input; namely that
-- the list of points passed is a contiguous chain, and that they are all
-- empty.
emptyStatus :: (R.Shape sh, Player p) => Board sh p -> [sh] -> Territory p
emptyStatus b ch = determineStatus Undecided surroundingPlayers
  where surroundingPlayers               = mapMaybe (asArray b !) . surrounding b $ ch
        determineStatus status    []     = status
        determineStatus Undecided (p:ps) = determineStatus (SurroundedBy p) ps
        determineStatus (SurroundedBy p1) (p2:ps)
                             | p1 == p2  = determineStatus (SurroundedBy p1) ps
                             | otherwise = Undecided

-- | Returns a list of groups of stones adjacent to a point @x@ whose colour
-- differs from that of player @p@.
pressuredGroups :: (R.Shape sh, Player p)
                => Board sh p -- ^ The current state of the board
                -> p          -- ^ The player @p@ making the threat
                -> sh         -- ^ The position @x@ where @p@ will play
                -> [[sh]]     -- ^ List of pressured groups
pressuredGroups b p = nub . map (chain b) . filter enemy . within b . neighbours
  where enemy x = asArray b ! x `notElem` [Nothing, Just p]

-- | Returns the state of the board after placing a stone of colour @p@ in
-- position @x@, along with a list of stones taken as a result.  Does not
-- check the legality of the move.
placeStone :: (R.Shape sh, Player p)
           => Board sh p -- ^ The current state of the board
           -> p          -- ^ The player, @p@
           -> sh         -- ^ The position, @x@
           -> ([(p, sh)], Board sh p) -- ^ List of taken stones and the
                                      -- resulting board position
placeStone b p x = (killedStones ++ zip (repeat p) suicidePoints, b''')
  where b'             = set b [x] (Just p)
        b''            = set b' killedPoints Nothing
        b'''           = set b'' suicidePoints Nothing
        killedPoints   = concat . filter noLiberties $ pressuredGroups b' p x
        killedStones   = map pairWithPlayer killedPoints
        pairWithPlayer = (,) . fromJust =<< (asArray b' !)
        noLiberties    = null . liberties b'
        placedGroup    = chain b'' x
        suicidePoints  = if null (liberties b'' placedGroup)
                           then placedGroup
                           else []
