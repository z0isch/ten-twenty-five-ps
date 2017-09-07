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
        [ HH.h1
          [ HP.classes [HH.ClassName "ui centered header"] ]
          [ HH.text "1025"]
        , HH.a 
            [ HP.classes [HH.ClassName "ui primary fluid button"]
            , HP.href "#/playGame"
            ]  [HH.text "New Game"]
        , HH.a 
            [ HP.classes [HH.ClassName "ui green fluid button"]
            , HP.href "#/stats"
            ]  [HH.text "Stats"]
        , HH.a 
            [ HP.classes [HH.ClassName "ui violet fluid button"]
            , HP.href "#/about"
            ]  [HH.text "About"]
        ]
      eval :: Query ~> H.ComponentDSL State Query Message m
      eval (Nonsense next) = pure next
