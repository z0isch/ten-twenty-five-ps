module Routes.PlayGame where
  
import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Now (NOW, nowDateTime)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Window (localStorage)
import Data.Array (length, replicate, snoc, zip, (..))
import Data.Lens.Index (ix)
import Data.Lens.Setter (set)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import GameSave (saveGame)
import GameTypes (Game, GameSave(..), initialGame, scoreGame)
import Halogen (liftEff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Round as Round

type State = Game

data Query a 
  = HandleRound Slot Round.Message a
  | Save a

type Slot = Int

type Input = Unit

data Message = GameSaved GameSave

ui :: forall e. H.Component HH.HTML Query Input Message (Aff (now :: NOW, dom :: DOM | e))
ui =  H.parentComponent
    { initialState: const initialGame
    , render
    , eval
    , receiver: const Nothing
    }
  where
    render :: State -> H.ParentHTML Query Round.Query Slot (Aff (now :: NOW, dom :: DOM | e))
    render state = HH.div
      [ HP.classes [HH.ClassName "ui container"]]
      [ HH.div
        [ HP.classes [HH.ClassName "ui eight column grid"]]
        $ snoc (snoc (snoc roundRows divider) totalRow) buttonRow
      ]
      where 
        roundRows = map roundSlot $ zip (0..length state) state
        roundSlot (Tuple i r) = HH.slot i (Round.ui r) unit (HE.input $ HandleRound i)
        totalRow = HH.div 
          [ HP.classes [HH.ClassName "row"]] 
          $ snoc (replicate 7 blankCol) totalScore
        blankCol = HH.div [HP.classes [HH.ClassName "column"]] []
        divider = HH.div [HP.classes [HH.ClassName "ui divider"]] []
        totalScore = HH.div 
          [HP.classes [HH.ClassName "column"]] 
          [HH.h1
            [HP.classes [HH.ClassName "stat"]] 
            [HH.text $ show (scoreGame state)]
          ]
        buttonRow = HH.div
          [ HP.classes [HH.ClassName "row"]]
          [ HH.button
            [ HP.classes [HH.ClassName "ui primary huge button"]
            , HE.onClick (HE.input_ Save)
            ]
            [ HH.text "Save"]
          , HH.a 
            [ HP.classes [HH.ClassName "ui huge button"]
            , HP.href "#"
            ]  [HH.text "Cancel"]
          ]

    eval :: Query ~> H.ParentDSL State Query Round.Query Slot Message (Aff (now :: NOW, dom :: DOM | e))
    eval (HandleRound i (Round.RoundChange r) next) = do
      state <- H.get
      H.put $ set (ix i) r state
      pure next
    eval (Save next) = do
        state <- H.get
        d <- liftEff nowDateTime
        ls <- liftEff $ window >>= localStorage
        let gs = GameSave {playedOn:d, game:initialGame}
        liftEff $ saveGame ls gs
        H.raise $ GameSaved gs
        pure next








