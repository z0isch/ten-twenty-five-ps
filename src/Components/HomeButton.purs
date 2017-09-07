module Components.HomeButton where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

homeButton :: forall a. H.ComponentHTML a
homeButton = HH.a 
  [ HP.classes [HH.ClassName "ui labeled icon button"]
  , HP.href "#/"
  ]  
  [ HH.i
    [ HP.classes [HH.ClassName "left chevron icon"] ]
    []
  , HH.text "Home"
  ]