-- | Monadic helpers to make working with go games easier.
--
-- These basically wrap the game inside the 'State' Monad, allowing you to
-- write things like (assuming you've imported 'Game.Go.Simple':
--
-- > game = do
-- >   black 17 3
-- >   white 2  16
-- >   black 3  3

module Game.Go.Monad
  ( Go
  , goWithSize
  , goWithState

  -- * Monadic helpers
  , setM
  , playMoveM
  , boardM
  , surroundingM
  , matchingNeighboursM
  , chainM
  , libertiesM
  , pressuredGroupsM
  ) where

import Control.Monad
import Control.Monad.State
import Control.Applicative       ((<$>))

import qualified Data.Array.Repa as R

import Game.Go

-- | The 'Go' monad wraps a 'Game' in the 'State' monad.
type Go sh p a = State (Game sh p) a

-- | Run the supplied go moves/analysis on a board of a given size,
-- returning the result and the state of the game after any moves played.
goWithSize :: (R.Shape sh, Player p)
           => Rules sh p -> sh -> Go sh p a -> (a, Game sh p)
goWithSize r dim g = goWithState g $ initGame r dim

-- | Take an initial game state, and run the supplied go moves/analysis on
-- that.  Returns the result and any resulting state.
goWithState :: Go sh p a -> Game sh p -> (a, Game sh p)
goWithState = runState

-- | Monadic version of 'set'.  Sets the value of a list of co-ordinates.
setM :: R.Shape sh => [sh] -> Maybe p -> Go sh p ()
setM coord v = do
    game <- get
    put $ game { board = set (board game) coord v }

-- | Monadic version of 'playMove'.  | Plays a move, returning the
-- resulting game state if the move was legal.
playMoveM :: (R.Shape sh, Player p)
          => p -> sh -> Go sh p (Either Illegal ())
playMoveM p x = do
    g <- get
    case playMove g p x of
      Left  e  ->           return (Left e)
      Right g' -> put g' >> return (Right ())

-- | Monadic version of 'board'.  Gets the current state of the board out
-- of a game state.
boardM :: Go sh p (Board sh p)
boardM = board <$> get

-- | Monadic version of 'surrounding'.  Gets the list of surrounding stones
-- from a point.
surroundingM :: R.Shape sh => [sh] -> Go sh p [sh]
surroundingM xs = flip surrounding xs <$> boardM

-- | Monadic version of 'matchingNeighbours'.  Gets the list of surrounding
-- stones which match the type passed in.
matchingNeighboursM :: (R.Shape sh, Player p) => Maybe p -> [sh] -> Go sh p [sh]
matchingNeighboursM v xs = liftM (\b -> matchingNeighbours b v xs) boardM

-- | Monadic version of 'chain'.  Returns the co-ordinates of all points in
-- the chain of which the supplied point is a member.
chainM :: (R.Shape sh, Player p) => sh -> Go sh p [sh]
chainM x = flip chain x <$> boardM

-- | Monadic version of 'liberties'.
libertiesM :: (R.Shape sh, Player p) => [sh] -> Go sh p [sh]
libertiesM x = flip liberties x <$> boardM

-- | Monadic version of 'pressuredGroups'.  Returns a list of groups of
-- stones adjacent to a point @x@ whose colour differs from that of player
-- @p@.
pressuredGroupsM :: (R.Shape sh, Player p)
                 => p          -- ^ The player p making the threat
                 -> sh         -- ^ The position x where p will play
                 -> Go sh p [[sh]]
pressuredGroupsM p x = liftM (\b -> pressuredGroups b p x) boardM
