module DOM.WebStorage.Game where

import Prelude

import Control.Coroutine (Producer)
import Control.Coroutine.Aff (produce)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Data.Either (Either(Left))
import TenTwentyFive.Types (GameSave)

foreign import savedGamesChange :: forall e. ((Array GameSave) -> Eff e Unit) -> Eff e Unit
foreign import saveGame :: forall e. GameSave -> Eff e Unit
foreign import deleteSavedGames :: forall e. Eff e Unit

savedGamesProducer :: forall eff. Producer (Array GameSave) (Aff (avar :: AVAR | eff)) Unit
savedGamesProducer = produce \emit -> savedGamesChange \r -> emit $ Left $ r