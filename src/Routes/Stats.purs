module Routes.Stats where

import Prelude

import Components.HomeButton (homeButton)
import Control.Monad.Aff (Aff)
import DOM (DOM)
import DOM.WebStorage.Game (deleteSavedGames)
import Data.Array (length)
import Halogen (liftEff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import TenTwentyFive.Types (GameSave)

type State = Array GameSave

data Query a 
  = HandleInput State a
  | ClearSavedGames a

type Input = State 

type Message = Void

ui :: forall e. H.Component HH.HTML Query Input Message (Aff (dom :: DOM | e))
ui =  H.component
        { initialState: \i -> i
        , render
        , eval
        , receiver: HE.input HandleInput
        }
    where
      render :: State -> H.ComponentHTML Query
      render gs = HH.div_
        [ homeButton
        , HH.div
          [ HP.classes [HH.ClassName "ui container"]]
          [ HH.h1
            [ HP.classes [HH.ClassName "ui centered header"] ]
            [HH.text "Stats"]
          , HH.text $ show $ length gs
          , HH.button
              [ HP.classes [HH.ClassName "ui red fluid button"]
              , HE.onClick $ HE.input_ $ ClearSavedGames
              ]  [HH.text "Delete Stats"]
          ]
        ]
      eval :: Query ~> H.ComponentDSL State Query Message (Aff (dom :: DOM | e))
      eval (HandleInput n next) = do
        H.put n
        pure next
      eval (ClearSavedGames next) = do
        liftEff $ deleteSavedGames
        pure next