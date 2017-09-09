module Main where

import Prelude

import Control.Coroutine (Consumer, consumer, runProcess, ($$))
import Control.Monad.Aff (Aff, forkAff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Control.Monad.Eff.Exception (EXCEPTION, error, throwException)
import Control.Monad.Eff.Now (NOW)
import Control.Monad.Error.Class (throwError)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToDocument)
import DOM.HTML.Window (document)
import DOM.Node.Document (createElement, getElementsByClassName, getElementsByTagName)
import DOM.Node.Element (setId)
import DOM.Node.HTMLCollection as C
import DOM.Node.Node (insertBefore, parentElement)
import DOM.Node.ParentNode (QuerySelector(..))
import DOM.Node.Types (Element, ElementId(..), Node, elementToNode)
import DOM.WebStorage.Game (savedGamesProducer)
import Data.Maybe (Maybe(..), maybe)
import Halogen.Aff (HalogenEffects, awaitBody, awaitLoad, runHalogenAff, selectElement)
import Halogen.VDom.Driver (runUI)
import Page (Message(..))
import Page as P
import Routes as R
import Routing.Hash (setHash)

makeWrapper :: forall e. Eff (dom :: DOM | e) (Maybe ElementId)     
makeWrapper = do
  htmlDoc <- window >>= document
  let doc = htmlDocumentToDocument htmlDoc
  scripts <- getElementsByTagName "script" doc
  l <- C.length scripts
  mEl <- map elementToNode <$> C.item (l-1) scripts 
  case mEl of
    Nothing -> pure Nothing
    (Just currScript) -> do
      mparEl <- parentElement currScript
      case mparEl of
        Nothing -> pure Nothing
        (Just parEl) -> do
          wrapper <- createElement "div" doc 
          setId (ElementId "test") wrapper
          _ <- insertBefore (elementToNode wrapper) currScript (elementToNode parEl)
          pure (Just $ ElementId "test")

main :: Eff (HalogenEffects (now :: NOW, dom :: DOM, console :: CONSOLE)) Unit
main = runHalogenAff do
  mWrapId <- liftEff makeWrapper
  wrapId <- maybe (throwError (error "Couldn't make wrapper")) pure mWrapId
  mWrap <- selectElement (QuerySelector "#test")
  wrap <- maybe (throwError (error "Couldn't find wrapper")) pure mWrap
  app <- runUI P.ui unit wrap
  _ <- forkAff $ runProcess (R.routeProducer $$ P.routeConsumer app.query)
  _ <- forkAff $ runProcess (savedGamesProducer $$ P.savedGamesConsumer app.query)
  app.subscribe pageMsgConsumer

pageMsgConsumer :: forall eff. Consumer P.Message (Aff (avar :: AVAR, dom :: DOM | eff)) Unit
pageMsgConsumer = consumer \event -> do
  case event of
    RouteTo s -> do
      liftEff $ setHash s
      pure Nothing























