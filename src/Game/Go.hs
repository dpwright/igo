-- | Base import.  Imports the flexible version of the library which uses
-- Repa co-ordinates for access (allowing you to express moves in greater
-- than 3 dimensions and to perform analysis on the underlying Repa
-- representation of the board, if you want).
--
-- If you want a simpler interface using (x, y) tuples and/or don't want to
-- pull in the Repa dependency, import "Game.Go.Simple" instead.

module Game.Go
  ( module Game.Go.Game
  , module Game.Go.Board
  , module Game.Go.Rules
  ) where

import Game.Go.Game
import Game.Go.Board
import Game.Go.Rules
