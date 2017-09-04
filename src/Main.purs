module Main where

import Prelude
import Types

import Control.Monad.Eff (Eff)
import Data.Array (head)
import Data.Maybe (fromMaybe)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Game as G

main :: Eff (HA.HalogenEffects ()) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI G.game unit body  