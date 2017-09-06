module DOM.WebStorage.Game where

import Prelude

import Control.Monad.Eff (Eff)
import DOM (DOM)
import DOM.WebStorage.Storage (getItem, setItem)
import DOM.WebStorage.Types (Storage)
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Encode (encodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Array ((:))
import Data.Either (either)
import Data.Maybe (maybe)
import TenTwentyFive.Types (GameSave)

clearSavedGames :: forall e. Storage -> Eff ( dom :: DOM | e) Unit
clearSavedGames ls = setItem "savedGames" "" ls

getSavedGames :: forall e. Storage -> Eff ( dom :: DOM | e) (Array GameSave)
getSavedGames ls = do
  games <- maybe "" id <$> getItem "savedGames" ls
  pure $ either (const []) (either (const []) id <<< decodeJson) $ jsonParser games
  
saveGame :: forall e. Storage -> GameSave -> Eff ( dom :: DOM | e) Unit
saveGame ls g = do
  games <- getSavedGames ls
  let json = encodeJson $ g : games
  setItem "savedGames" (stringify json) ls













