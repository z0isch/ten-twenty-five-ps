module Routes.Home where

import Prelude

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
        [ HH.a 
          [ HP.classes [HH.ClassName "ui primary button"]
          , HP.href "#/playGame"
          ]  [HH.text "New Game"]
        ]
      eval :: Query ~> H.ComponentDSL State Query Message m
      eval (Nonsense next) = pure next
