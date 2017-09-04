module Game where
  
import Prelude

import Data.Array (concat, length, snoc, zip, (..))
import Data.Lens.Index (ix)
import Data.Lens.Setter (set)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Round as Round
import Types (Game, initialGame, scoreGame)

type State = Game

data Query a = HandleRound Slot Round.Message a

type Slot = Int

game :: forall m.  H.Component HH.HTML Query Unit Void m
game =  H.parentComponent
    { initialState: const initialGame
    , render
    , eval
    , receiver: const Nothing
    }
  where
    render :: State -> H.ParentHTML Query Round.Query Slot m 
    render state = HH.div
      [ HP.classes [HH.ClassName "ui container"]]
      [ HH.div
        [ HP.classes [HH.ClassName "ui seven column grid"]]
        (snoc roundRows totalRow)
      ]
      where 
        roundRows = map roundSlot $ zip (0..length state) state
        roundSlot (Tuple i r) = HH.slot i (Round.round r) unit (HE.input $ HandleRound i)
        totalRow = HH.div 
          [ HP.classes [HH.ClassName "row"]] 
          [ HH.div [HP.classes [HH.ClassName "column"]] [], HH.div [HP.classes [HH.ClassName "column"]] [], HH.div [HP.classes [HH.ClassName "column"]] [], HH.div [HP.classes [HH.ClassName "column"]] [], HH.div [HP.classes [HH.ClassName "column"]] [], HH.div [HP.classes [HH.ClassName "column"]] []
          , HH.div [HP.classes [HH.ClassName "column"]] [HH.text $ show $ scoreGame state]
          ]

    eval :: Query ~> H.ParentDSL State Query Round.Query Slot Void m
    eval (HandleRound i (Round.RoundChange r) next) = do
      state <- H.get
      H.put $ set (ix i) r state
      pure next