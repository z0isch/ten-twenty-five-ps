module Routes.History where

import Prelude

import Data.Array (length)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import TenTwentyFive.Types (GameSave(..))

type State = Array GameSave

data Query a = HandleInput State a

type Input = State 

type Message = Void

ui :: forall m. H.Component HH.HTML Query Input Message m
ui =  H.component
        { initialState: \i -> i
        , render
        , eval
        , receiver: HE.input HandleInput
        }
    where
      render :: State -> H.ComponentHTML Query
      render [] =  HH.div_ 
        [HH.text "No history."]
      render gs = HH.div_ 
        [HH.text $ show $ length gs]
      eval :: Query ~> H.ComponentDSL State Query Message m
      eval (HandleInput n next) = do
        H.put n
        pure next

