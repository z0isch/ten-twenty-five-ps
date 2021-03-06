module Routes.Stats where

import Prelude

import Components.HomeButton (homeButton)
import Control.Monad.Aff (Aff)
import DOM (DOM)
import DOM.WebStorage.Game (deleteSavedGames)
import Data.Array (length)
import Data.Int (round)
import Halogen (liftEff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import TenTwentyFive.Types (GameSave, averageRoundPercents, averageScore, highestScore)

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
          , stat "small" (show $ length gs) "Games Played"
          , stat "small" (show $ round $ averageScore gs) "Avg Score"
          , stat "small" (show $ round $ highestScore gs) "Highest Score"
          , HH.div [HP.classes [HH.ClassName "ui divider"]] []
          , HH.div_ [ roundPercentTable $ averageRoundPercents gs ]
          , HH.button
              [ HP.classes [HH.ClassName "ui red fluid button"]
              , HE.onClick $ HE.input_ $ ClearSavedGames
              ]  [HH.text "Delete Stats"]
          ]
        ]
      roundPercentTable rps = HH.table
        [ HP.classes [HH.ClassName "ui very basic celled table"]]
        [ HH.thead_ 
          [ HH.tr_
              [ HH.td [HP.classes 
                [ HH.ClassName "collapsing"]] 
                [HH.text "Distance"]
              , HH.td [HP.classes 
                [ HH.ClassName "collapsing"]] 
                [HH.text "Make %"]
              ]
          ]
          , HH.tbody_ $ map roundPercentRow rps
        ]
      roundPercentRow {distance:d, average:a} = HH.tr_ 
        [ HH.td 
          [HP.classes [ HH.ClassName "collapsing"]]
          [HH.text $ show d <> "'"]
        , HH.td 
            [HP.classes [ HH.ClassName "collapsing"]] 
            [HH.text $ percent a]
        ] 
      percent a = (show $ round $ a * 100.0) <> "%"
      stat t v l =  HH.div
        [ HP.classes [HH.ClassName $ "ui "<> t <>" statistic"]]
        [ HH.div
          [ HP.classes [HH.ClassName "value"]]
          [HH.text v]
        , HH.div
          [ HP.classes [HH.ClassName "label"]]
          [HH.text l]
        ]
      eval :: Query ~> H.ComponentDSL State Query Message (Aff (dom :: DOM | e))
      eval (HandleInput n next) = do
        H.put n
        pure next
      eval (ClearSavedGames next) = do
        liftEff $ deleteSavedGames
        pure next












