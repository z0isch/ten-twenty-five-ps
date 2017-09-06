module DOM.WebStorage.Game where

import Prelude

import Control.Coroutine (Producer, emit)
import Control.Coroutine.Aff (produce)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import DOM (DOM)
import DOM.WebStorage.Storage (getItem, setItem)
import DOM.WebStorage.Types (Storage)
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Encode (encodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Array ((:))
import Data.Either (Either(..), either)
import Data.Maybe (maybe)
import TenTwentyFive.Types (GameSave)

foreign import savedGamesChange :: forall e. (String -> Eff e Unit) -> Eff e Unit
foreign import saveGameLS :: forall e. String -> Eff e Unit

saveGame :: forall e. GameSave -> Eff ( dom :: DOM | e) Unit
saveGame g = saveGameLS $ stringify $ encodeJson $ g

parseSavedGames :: String -> Array GameSave
parseSavedGames = either (const []) (either (const []) id <<< decodeJson) <<< jsonParser

savedGamesProducer :: forall eff. Producer (Array GameSave) (Aff (avar :: AVAR | eff)) Unit
savedGamesProducer = produce \emit -> savedGamesChange \r -> emit $ Left $ parseSavedGames r







