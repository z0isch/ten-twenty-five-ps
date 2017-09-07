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
      render _ =   HH.div_
        [ homeButton
        , HH.div
          [ HP.classes [HH.ClassName "ui container"]]
          [ HH.h1
            [ HP.classes [HH.ClassName "ui centered header"] ]
            [HH.text "About"]
          , HH.h2_ [ HH.text "What is 1025?"]
          , HH.p_ [HH.text "1025 is a skills-based program designed to improve your disc golf putt and enhance competitive play.  A complete round consists of 36 putts measured from 10’ to 35’, tiered in 5’ intervals.  A perfect round will result in a score of 1025."]
          , HH.h2_ [ HH.text "How Do You Play 1025?"]
          , HH.p_ [HH.text "Measure and mark the putting distances from the basket.  Throw 6 putts from each tier.  In order to account for variances in wind condition, lighting, and ground slope, no more than 2 putts may be thrown from a single location.  It is suggested that you set up a triangle for each tier and throw 2 putts from each location."]
          ]
        ]
      eval :: Query ~> H.ComponentDSL State Query Message m
      eval (Nonsense next) = pure next