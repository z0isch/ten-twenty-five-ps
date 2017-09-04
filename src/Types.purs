module Types where

import Data.Array
import Prelude

import Data.Foldable (and, sum)
import Data.Generic (class Generic, gShow)
import Data.Maybe (fromMaybe)

newtype Round = Round { results :: Array Boolean, distance :: Int }
derive instance genericRound :: Generic Round
instance showRound :: Show Round where
  show = gShow

type Game = Array Round

initialGame :: Game
initialGame = [
    Round { results: [false,false,false,false,false,false], distance: 10},
    Round { results: [false,false,false,false,false,false], distance: 15},
    Round { results: [false,false,false,false,false,false], distance: 20},
    Round { results: [false,false,false,false,false,false], distance: 25},
    Round { results: [false,false,false,false,false,false], distance: 30},
    Round { results: [false,false,false,false,false,false], distance: 35}
]

scoreGame :: Game -> Int
scoreGame = sum <<< map scoreRound

scoreRound :: Round -> Int
scoreRound r = madeShotScore r + madeAllScore r + madeFirstLastScore r

madeFirstLastScore :: Round -> Int
madeFirstLastScore (Round r) = madeScore head + madeScore last
  where
    madeScore f = multiplier * boolToInt (fromMaybe false (f r.results))
    multiplier
      | r.distance < 30 = 5
      | otherwise = 10

madeAllScore :: Round -> Int
madeAllScore (Round r) = r.distance * boolToInt (and r.results)

boolToInt :: Boolean -> Int
boolToInt true = 1
boolToInt _ = 0

madeShotScore :: Round -> Int
madeShotScore (Round r) = r.distance * getMadeBaskets (Round r)

getMadeBaskets :: Round -> Int
getMadeBaskets (Round r) = sum $ map boolToInt r.results


