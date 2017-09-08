module Round where

import Prelude

import Data.Array (length, snoc, zip, (..), (:))
import Data.Lens.Index (ix)
import Data.Lens.Setter (over)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import TenTwentyFive.Types (Round(..), scoreRound)

type State = Round

data Query a 
    = Toggle Int a

data Message = RoundChange Round

type Input = Unit

ui :: forall m. Round -> H.Component HH.HTML Query Input Message m
ui r =
    H.component
        { initialState: const r
        , render
        , eval
        , receiver: const Nothing
        }
    where
        render :: State -> H.ComponentHTML Query
        render (Round r') = HH.div
            [HP.classes [HH.ClassName "row"]]
            (distanceCol:snoc tossCols scoreCol)  
            where
                distanceCol = HH.div 
                    [HP.classes [HH.ClassName "column"]] 
                    [HH.h2 [HP.classes [HH.ClassName "stat"]] [HH.text $ (show r'.distance) <> "'"]]
                tossCols = map toss (zip (0..length r'.results) r'.results)
                scoreCol = HH.div 
                    [HP.classes [HH.ClassName "column"]] 
                    [HH.h2 [HP.classes [HH.ClassName "stat"]] [HH.text $ show $ scoreRound (Round r')]]
        toss (Tuple i b) = HH.div
            [ HP.classes [HH.ClassName "column"]]
            [ HH.input 
                [ HP.type_ HP.InputCheckbox
                , HP.checked b
                , HE.onChecked $ HE.input_ $ Toggle i
                , HP.classes [HH.ClassName "ui checkbox round"]
                ]
            ]
        eval :: Query ~> H.ComponentDSL State Query Message m
        eval (Toggle i next) = do
            Round r' <- H.get
            let newResults = over (ix i) not r'.results
            let newRound = Round {results:newResults, distance: r'.distance}
            H.put newRound
            H.raise $ RoundChange newRound
            pure next