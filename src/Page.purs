module Page where

import Prelude
import Routes (Routes(..))

import Control.Coroutine (Consumer, consumer)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Data.Maybe (Maybe(..))
import Halogen (modify)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Game as G

type State = {currentPage :: Routes}

data Query a
  = Goto Routes a

ui :: forall m. H.Component HH.HTML Query Unit Void m
ui = H.component 
  { initialState: const {currentPage: Home}
        , render
        , eval
        , receiver: const Nothing
  }
  where
    render :: State -> H.ComponentHTML Query
    render st =
      HH.div_
        [ HH.h1_ [ HH.text (show $ st.currentPage) ]
        , HH.p_ [ HH.text "Routing!!" ]
        ]
    eval :: Query ~> H.ComponentDSL State Query Void m
    eval (Goto Home next) = do
      modify (_{ currentPage = Home })
      pure next
    eval (Goto PlayGame next) = do
      modify (_{ currentPage = PlayGame })
      pure next


routeConsumer :: forall a eff.                                                           
  (Query Unit -> Aff (avar :: AVAR | eff) a) -> 
  Consumer Routes (Aff (avar :: AVAR | eff)) Unit
routeConsumer q = consumer \event -> do
  _ <- q $ (H.action <<< Goto) event
  pure Nothing