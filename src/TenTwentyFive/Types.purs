module TenTwentyFive.Types where

import Prelude

import Data.Argonaut.Core (foldJsonArray, toArray)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Decode.Generic (gDecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Generic (gEncodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Array (concat, filter, groupBy, head, last, length, replicate, (:))
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

type Round = { results :: Array Boolean, distance :: Distance }

type Game = Array Round

type GameSave = {playedOn :: LocalDateTime , game :: Game}

averageRoundPercents :: (Array GameSave) -> Array {distance:: Distance, average:: Number}
averageRoundPercents gs = map mkPoint grouped
  where rounds = concat $ map (\{game:g} -> g) gs
        grouped = groupBy (\{distance:a} {distance:b} -> a == b) rounds
        totalRoundShots = sum <<< map (toNumber <<< roundLength) <<< NE.fromNonEmpty (\a fa -> a : fa)
        roundAvg = sum <<< map (toNumber <<< madeBaskets) <<< NE.fromNonEmpty (\a fa -> a : fa)
        mkPoint g = {distance: (NE.head g).distance, average: roundAvg g / (totalRoundShots g)}

madeBaskets :: Round -> Int
madeBaskets ({results:r}) = length $ filter ((==) true) r
roundLength :: Round -> Int
roundLength {results:r} = length r

scores :: (Array GameSave) -> (Array Number)
scores = map (\{game:g} -> toNumber $ scoreGame g)

highestScore :: (Array GameSave) -> Number
highestScore [] = 0.0
highestScore gs = maximum $ scores gs

averageScore :: (Array GameSave) -> Number
averageScore [] = 0.0
averageScore gs = mean $ scores gs

initialGame :: Game
initialGame = [
    { results: replicate 6 false, distance: 10},
    { results: replicate 6 false, distance: 15},
    { results: replicate 6 false, distance: 20},
    { results: replicate 6 false, distance: 25},
    { results: replicate 6 false, distance: 30},
    { results: replicate 6 false, distance: 35}
]

scoreGame :: Game -> Int
scoreGame = sum <<< map scoreRound

scoreRound :: Round -> Int
scoreRound r = madeShotScore r + madeAllScore r + madeFirstLastScore r

madeFirstLastScore :: Round -> Int
madeFirstLastScore r = madeScore head + madeScore last
  where
    madeScore f = multiplier * boolToInt (fromMaybe false (f r.results))
    multiplier
      | r.distance < 30 = 5
      | otherwise = 10

madeAllScore :: Round -> Int
madeAllScore r = r.distance * boolToInt (and r.results)

boolToInt :: Boolean -> Int
boolToInt true = 1
boolToInt _ = 0

madeShotScore :: Round -> Int
madeShotScore r = r.distance * getMadeBaskets r

getMadeBaskets :: Round -> Int
getMadeBaskets r = sum $ map boolToInt r.results

















