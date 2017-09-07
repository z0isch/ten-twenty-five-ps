module Routes.About where

import Prelude

import Components.HomeButton (homeButton)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type State = Unit

data Query a = Nonsense a

type Input = Unit

type Message = Void

ui :: forall m. H.Component HH.HTML Query Input Message m
ui =  H.component
        { initialState: const unit
        , render
        , eval
        , receiver: const Nothing
        }
    where
      render :: State -> H.ComponentHTML Query
      render _ =  HH.div_ 
        [ homeButton
        , HH.h1_ [HH.text "About"]
        ]
      eval :: Query ~> H.ComponentDSL State Query Message m
      eval (Nonsense next) = pure next