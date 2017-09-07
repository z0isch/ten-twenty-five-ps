module Main where

import Prelude

import Control.Coroutine (Consumer, consumer, runProcess, ($$))
import Control.Monad.Aff (Aff, forkAff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Now (NOW)
import DOM (DOM)
import DOM.WebStorage.Game (savedGamesProducer)
import Data.Maybe (Maybe(..))
import Halogen.Aff (HalogenEffects, awaitBody, runHalogenAff)
import Halogen.VDom.Driver (runUI)
import Page (Message(..))
import Page as P
import Routes as R
import Routing.Hash (setHash)

main :: Eff (HalogenEffects (now :: NOW, dom :: DOM, console :: CONSOLE )) Unit
main = runHalogenAff do
  body <- awaitBody
  app <- runUI P.ui unit body
  _ <- forkAff $ runProcess (R.routeProducer $$ P.routeConsumer app.query)
  _ <- forkAff $ runProcess (savedGamesProducer $$ P.savedGamesConsumer app.query)
  app.subscribe pageMsgConsumer

pageMsgConsumer :: forall eff. Consumer P.Message (Aff (avar :: AVAR, dom :: DOM | eff)) Unit
pageMsgConsumer = consumer \event -> do
  case event of
    RouteTo s -> do
      liftEff $ setHash s
      pure Nothing