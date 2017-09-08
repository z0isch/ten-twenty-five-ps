module TenTwentyFive.Types where

import Prelude

import Data.Argonaut.Core (foldJsonArray, toArray)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Decode.Generic (gDecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Generic (gEncodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Array (concat, concatMap, filter, groupBy, head, last, length, replicate, sortWith, (:))
import Data.DateTime.Locale (LocalDateTime)
import Data.Either (either)
import Data.Foldable (and, sum)
import Data.Generic (class Generic, gShow)
import Data.Int (toNumber)
import Data.Lens (Fold)
import Data.Maybe (fromMaybe, maybe)
import Data.NonEmpty (NonEmpty(..))
import Data.NonEmpty as NE
import Data.Tuple (Tuple(..))
import Math.Statistics.Unsafe (maximum, mean)

type Distance = Int

data Round = Round { results :: Array Boolean, distance :: Distance }
derive instance genericRound :: Generic Round
instance showMyRound :: Show Round where
    show = gShow

type Game = Array Round

data GameSave = GameSave {playedOn :: LocalDateTime , game :: Game}
derive instance genericGameSave :: Generic GameSave
instance showGameSave :: Show GameSave where
    show = gShow

averageRoundPercents :: (Array GameSave) -> Array {distance:: Distance, average:: Number}
averageRoundPercents gs = map mkPoint $ grouped gs
  where
        mkPoint g = {distance: getDistance (NE.head g), average: roundAvg g / totalRoundShots g}
        totalRoundShots = sumIt (\(Round r) -> length r.results)
        roundAvg = sumIt (\(Round r) -> length $ filter ((==) true) r.results)
        sumIt f =  sum <<< map (toNumber <<< f) <<< NE.fromNonEmpty (\a fa -> a : fa)

getDistance :: Round -> Int
getDistance (Round r) = r.distance

grouped :: Array GameSave -> Array (NonEmpty Array Round)
grouped = groupBy equalRs <<< sortWith getDistance <<< concatMap (\(GameSave g) -> g.game)
  where
    equalRs (Round r1) (Round r2) = r1.distance == r2.distance 

scores :: (Array GameSave) -> (Array Number)
scores = map (\(GameSave {playedOn:_ , game : g}) -> toNumber $ scoreGame g)

highestScore :: (Array GameSave) -> Number
highestScore [] = 0.0
highestScore gs = maximum $ scores gs

averageScore :: (Array GameSave) -> Number
averageScore [] = 0.0
averageScore gs = mean $ scores gs

initialGame :: Game
initialGame = [
    Round { results: replicate 6 false, distance: 10},
    Round { results: replicate 6 false, distance: 15},
    Round { results: replicate 6 false, distance: 20},
    Round { results: replicate 6 false, distance: 25},
    Round { results: replicate 6 false, distance: 30},
    Round { results: replicate 6 false, distance: 35}
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



















