-- | Defines the various standard rulesets, and the rules used to make
-- them.  The following resources were immensely useful in determining what
-- to put in here:
--
-- * <http://www.britgo.org/rules/compare.html#bent        British Go Association rules comparison page>
--
-- * <http://online-go.com/docs/go-rules-comparison-matrix online-go.com rules comparison page>
--
-- * <http://senseis.xmp.net/?KoPagesTaxonomy#toc8         Sensei's Library wiki section on ko rules>
--
-- There are still a number of standard rulesets not included here, most
-- notably the Chinese ruleset.  This is, in general, because the /ko/
-- rules for those rulesets are quite difficult to formulate.  I have
-- attempted to include only those rulesets which I can accommodate
-- correctly, rather than have, for example, a Chinese ruleset which almost
-- works but doesn't quite treat <http://senseis.xmp.net/?SendingTwoReturningOne Sending two, returning one>
-- correctly, for example.

module Game.Go.Rules
  ( -- * Standard rulesets
    japanese
  , aga
  , newZealand

    -- * Ko rules
  , positional
  , simpleKo
  , psk
  , ssk

    -- * Komi rules
  , komiSubtractedFromFirstPlayer
  , komiDividedEqually
  , komiRampsDown

    -- * Endgame queries
  , everyoneHasPassed
  , currentPlayerIsFirstPlayer
  ) where

import Game.Go.Game
import Game.Go.Board

import qualified Data.Array.Repa as R

import Control.Applicative (liftA2)

-- | Utility to create ko checkers which rely only on board positions, not
-- whole game states
positional :: (R.Shape sh, Eq p) => ([Board sh p] -> Bool) -> Game sh p -> Bool
positional f g = f (map board $ g:history g)

-- | Simple Ko: A play may not recreate the last board position
simpleKo :: (R.Shape sh, Eq p) => [Board sh p] -> Bool
simpleKo (p:_:b':_) = b' == p
simpleKo _          = False

-- | Positional Super-Ko: A play may not recreate any previous board
-- position from the game
psk :: (R.Shape sh, Eq p) => [Board sh p] -> Bool
psk (b:allBoards) = b `elem` allBoards

-- | Situational Super-Ko.  Using the New Zealand definition here: \"It is
-- illegal to play so that the resulting board position repeats the whole
-- board position as it was after any of that player's previous moves\".
--
-- Under AGA rules, this definition is equivalent to the AGA definition as
-- suicides aren't allowed, but under New Zealand rules this allows
-- repetition of the starting board state, which AGA rules don't.  There is
-- no known ruleset where suicides are allowed but the empty board position
-- is not allowed to be repeated, so there is no need for a separate @ssk@
-- implementation covering the AGA wording.
ssk :: (R.Shape sh, Eq p) => Game sh p -> Bool
ssk g = (p, board g) `elem` allSituations
  where h              = history g
        allBoards      = map board h
        (p:allPlayers) = map toPlay h
        allSituations  = zip allPlayers allBoards

-- | This is the standard in most rulesets; Black gets a penalty for
-- playing first, and this is subtracted from his score.
komiSubtractedFromFirstPlayer :: Player p => p -> Float
komiSubtractedFromFirstPlayer p = if fromEnum p == 0 then (-1) else 0

-- | This is the standard under Chinese counting; Black gives half-/komi/'s
-- worth of stones to White, resulting in half of /komi/ being subtracted
-- from Black and half being awarded to White.  When playing with more than
-- two players, we just divide by the total number to keep things simple.
komiDividedEqually :: Player p => p -> Float
komiDividedEqually p = if fromEnum p == 0 then (-komiShare) else komiShare
  where numPlayers = fromEnum (maxBound `asTypeOf` p) + 1
        komiShare  = 1 / fromIntegral numPlayers

-- | This system is not part of any standard ruleset as it doesn't make
-- sense in two-player games, but it's included here as a first attempt to
-- invent a rule for /komi/ that would work with games of > 2 players.
-- /komi/ is subtractive, as in 'komiSubtractedFromFirstPlayer', but it is
-- divided amongst all the players, so that player 1's penalty is twice
-- player 2's, and so forth.  The last player receives no penalty.
--
-- In a 2-player game this should be equivalent to
-- 'komiSubtractedFromFirstPlayer'.
komiRampsDown :: Player p => p -> Float
komiRampsDown p = fromIntegral komiShare / fromIntegral numShares
  where numPenalties = fromEnum $ maxBound `asTypeOf` p
        shares       = reverse [0..numPenalties]
        numShares    = sum shares
        komiShare    = shares !! fromEnum p

-- | Check whether all players have passed, sequentially
everyoneHasPassed :: Player p => Game sh p -> Bool
everyoneHasPassed g = passes g > fromEnum (maxBound `asTypeOf` toPlay g) + 1

-- | Under AGA rules, the game ends wen everyone has passed and the last
-- player to pass was White; i.e. the current player is Black.  Here we
-- rephrase that as \"the current player is the first player\", which means
-- the same thing with a standard Black vs White game, but also works with
-- greater numbers of players.
currentPlayerIsFirstPlayer :: Enum p => Game sh p -> Bool
currentPlayerIsFirstPlayer g = fromEnum (toPlay g) == 0

-- | Standard Japanese rules.
japanese :: (R.Shape sh, Player p) => Rules sh p
japanese = Rules
         { komi           = 6.5
         , komiMultiplier = komiSubtractedFromFirstPlayer
         , ko             = positional simpleKo
         , allowSuicide   = False
         , isGameOver     = everyoneHasPassed
         }

-- | American Go Association rules.
aga :: (R.Shape sh, Player p) => Rules sh p
aga = Rules
    { komi           = 7.5
    , komiMultiplier = komiSubtractedFromFirstPlayer
    , ko             = ssk
    , allowSuicide   = False
    , isGameOver     = everyoneHasPassed <&&> currentPlayerIsFirstPlayer
    } where (<&&>)   = liftA2 (&&)

-- | Standard New Zealand rules.
newZealand :: (R.Shape sh, Player p) => Rules sh p
newZealand = Rules
           { komi           = 7
           , komiMultiplier = komiSubtractedFromFirstPlayer
           , ko             = ssk
           , allowSuicide   = True
           , isGameOver     = everyoneHasPassed
           }
