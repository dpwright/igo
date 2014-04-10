-- | Defines the data required to model an actual game of go, along with
-- functions to simulate playing a game.

module Game.Go.Game
  ( -- * Datatypes
    Game (..)
  , Rules (..)
  , Illegal (..)

    -- * Functions
  , initGame
  , initGameWithBoard
  , setPlayer
  , playMove
  , playPass
  , isLegal
  ) where

import qualified Data.Array.Repa as R
import qualified Data.Map        as Map

import Data.List                 (partition, foldl')
import Data.Maybe

import Game.Go.Board

-- | A game is defined by the current state of the board, and every state
-- that has led up to the current state.  The current player's turn and the
-- number of successive passes is also required.
data Game sh p = Game
             { -- | The current player.
               toPlay     :: p
               -- | The current state of the board.
             , board      :: Board sh p
               -- | All game states until the current point.
             , history    :: [Game sh p]
               -- | The number of stones taken from opposing players.
             , stonesWon  :: Map.Map p Int
               -- | The number of stones taken by opposing players.
             , stonesLost :: Map.Map p Int
               -- | The number of stones lost through suicide.
             , suicides   :: Map.Map p Int
               -- | The number of sequential passes.
             , passes     :: Int
               -- | The ruleset by which we're playing
             , rules      :: Rules sh p
             }

-- | The rules of the game are defined by simple values, such as the amount
-- of /komi/ or whether or not suicidal plays are allowed, or by a function
-- on the 'Game' state where deeper analysis is necessary.  In general, the 
-- 'Game' state passed in will be that just /after/ the move has been
-- played, so we can see whether the /resulting state/ is illegal.
--
-- Standard rulesets and the rules to create them are defined in the
-- 'Game.Go.Rules' module.
data Rules sh p = Rules
                  { -- | The penalty for starting first
                    komi           :: Float
                    -- | The amount of /komi/ to award to each player.
                    -- Under most rulesets, Black gets negative /komi/ and
                    -- White gets 0, but under Chinese rules /komi/ is
                    -- divided between the players.  Finally, the idea of
                    -- playing with more than two players opens up the
                    -- possibility for many non-standard treatments of
                    -- /komi/.  See "Game.Go.Rules" for details.
                  , komiMultiplier :: p -> Float
                    -- | Tells us whether the last move played results in /ko/
                  , ko             :: Game sh p -> Bool
                    -- | Can we play a move which results in our stones being taken?
                  , allowSuicide   :: Bool
                    -- | Game Over condition
                  , isGameOver     :: Game sh p -> Bool
                  }

-- | Datatype returned when attempting to play an illegal move.
data Illegal = Suicide       -- ^ Suicide, as defined by the current ruleset.
             | Ko            -- ^ Ko, as defined by the current ruleset.
             | Occupied      -- ^ The played position was already occupied.
             | WrongPlayer   -- ^ Tried to play out of order.
             | OutOfBounds   -- ^ Attempted to play a position outside the boundaries of the board.
             deriving (Eq, Show)

-- | Initialises a new game with the given board dimensions.
initGame :: (R.Shape sh, Player p) => Rules sh p -> sh -> Game sh p
initGame r dim = initGameWithBoard r $ empty dim

-- | Initialises a new game with a starting board (useful for handicap games).
initGameWithBoard :: (R.Shape sh, Player p) => Rules sh p -> Board sh p -> Game sh p
initGameWithBoard r b = Game
                    { toPlay      = toEnum 0
                    , board       = b
                    , history     = []
                    , stonesWon   = Map.empty
                    , stonesLost  = Map.empty
                    , suicides    = Map.empty
                    , passes      = 0
                    , rules       = r}

-- | Set the current player manually.
setPlayer :: Player p => Game sh p -> p -> Game sh p
setPlayer g p = g { toPlay = p }

-- | Play a move.  Checks the legality of the move, and if it is legal,
-- returns the game state following the move.
playMove :: (R.Shape sh, Player p)
         => Game sh p -> p -> sh -> Either Illegal (Game sh p)
playMove g p x
  | p /= toPlay g       = Left WrongPlayer
  | null $ within b [x] = Left OutOfBounds
  | isJust $ b `at` x   = Left Occupied
  | not suicideFree     = Left Suicide
  | isKo g'             = Left Ko
  | otherwise           = Right g'
  where b                  = board g
        isKo               = ko $ rules g
        (lostStones, b')   = placeStone b p x
        (myLoss, yourLoss) = partition ((== p) . fst) lostStones
        suicideFree        = allowSuicide (rules g) || null myLoss
        addToMap m k v     = Map.alter (Just . maybe v (+ v)) k m
        addEnemies m (k,_) = addToMap m k 1
        g' = g
           { toPlay     = next p
           , board      = b'
           , history    = g:history g
           , stonesWon  = addToMap (stonesWon g) p $ length yourLoss
           , stonesLost = foldl' addEnemies (stonesLost g) yourLoss
           , suicides   = addToMap (suicides g) p $ length myLoss
           , passes     = 0 }

-- | Play a pass.  The number of passes will be incremented.  The game is
-- over when all players have passed sequentially, and then the first
-- player to pass passes again.
playPass :: Player p => Game sh p -> p -> Either Illegal (Game sh p)
playPass g p | p /= toPlay g = Left WrongPlayer
             | otherwise     = Right $ g { passes = passes g + 1
                                         , toPlay = next $ toPlay g }

-- | Check the legality of a move.
isLegal :: (R.Shape sh, Player p)
        => Game sh p -> p -> sh -> Bool
isLegal g p = either (const False) (const True) . playMove g p

-- Wrapped enums, taken from: http://stackoverflow.com/questions/5684049/is-there-some-way-to-define-an-enum-in-haskell-that-wraps-around
next :: (Enum a, Bounded a) => a -> a
next = wrappedAdd 1

wrappedAdd :: (Enum a, Bounded a) => Int -> a -> a
wrappedAdd n e = toEnum (add (fromEnum (maxBound `asTypeOf` e) + 1) (fromEnum e) n)
  where add m x y = (x + y + m) `rem` m
