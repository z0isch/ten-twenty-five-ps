module Types where

import Prelude

import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Decode.Generic (gDecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Generic (gEncodeJson)
import Data.Array (head, last, replicate)
import Data.DateTime.Locale (LocalDateTime)
import Data.Foldable (and, sum)
import Data.Generic (class Generic, gShow)
import Data.Maybe (fromMaybe)

newtype Round = Round { results :: Array Boolean, distance :: Int }
derive instance genericRound :: Generic Round
instance showRound :: Show Round where
  show = gShow
instance encodeJsonRound :: EncodeJson Round where
  encodeJson = gEncodeJson
instance decodeJsonRound :: DecodeJson Round where
  decodeJson = gDecodeJson

type Game = Array Round

newtype GameSave = GameSave {playedOn :: LocalDateTime , game :: Game}
derive instance genericGameSave :: Generic GameSave
instance showGameSave :: Show GameSave where
  show = gShow
instance encodeJsonGameSave :: EncodeJson GameSave where
  encodeJson = gEncodeJson
instance decodeJsonGameSave :: DecodeJson GameSave where
  decodeJson = gDecodeJson

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
