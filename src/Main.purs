module Main where

import Prelude
import Types

import Control.Coroutine (Consumer, consumer, runProcess, ($$))
import Control.Coroutine.Aff (produce)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Control.Monad.Eff.Now (NOW, nowDateTime)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Window (localStorage)
import DOM.WebStorage.Storage (Storage, getItem, setItem)
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Encode (encodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Array ((:))
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), maybe)
import Game as G
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Page as P
import Routes (Routes)
import Routes as R

main = HA.runHalogenAff do
  body <- HA.awaitBody
  app <- runUI P.ui unit body
  runProcess (R.routeProducer $$ P.routeConsumer app.query)
  
-- main :: Eff (HA.HalogenEffects (console :: CONSOLE, now :: NOW)) Unit
-- main = HA.runHalogenAff do
--     body <- HA.awaitBody
--     ls <- liftEff $ window >>= localStorage
--     d <- liftEff nowDateTime
--     liftEff $ saveGame ls $ GameSave {playedOn:d, game:initialGame}
--     g <- liftEff $ getSavedGames ls
--     liftEff $ logShow g
--     liftEff $ clearSavedGames ls
--     runUI G.game unit body

clearSavedGames :: forall e. Storage -> Eff ( dom :: DOM | e) Unit
clearSavedGames ls = setItem "savedGames" "" ls

getSavedGames :: forall e. Storage -> Eff ( dom :: DOM | e) (Array GameSave)
getSavedGames ls = do
  games <- maybe "" id <$> getItem "savedGames" ls
  pure $ either (const []) (either (const []) id <<< decodeJson) $ jsonParser games
  
saveGame :: forall e. Storage -> GameSave -> Eff ( dom :: DOM | e) Unit
saveGame ls g = do
  games <- getSavedGames ls
  let json = encodeJson $ g:games
  setItem "savedGames" (stringify json) ls




























