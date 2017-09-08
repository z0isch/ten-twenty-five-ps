module Routes.PlayGame where
  
import Prelude

import Components.HomeButton (homeButton)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Now (NOW, nowDateTime)
import DOM (DOM)
import DOM.WebStorage.Game (saveGame)
import Data.Array (length, replicate, snoc, zip, (..))
import Data.Lens.Index (ix)
import Data.Lens.Setter (set)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Halogen (liftEff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Round as Round
import TenTwentyFive.Types (Game, GameSave(..), initialGame, scoreGame)

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
    render state = HH.div_
        [ homeButton
        , HH.div
          [ HP.classes [HH.ClassName "ui container"]]
          [ HH.h1
            [ HP.classes [HH.ClassName "ui centered header"] ]
            [HH.text "New Game"] 
          , HH.div
            [ HP.classes [HH.ClassName "ui eight column grid"]]
            $ snoc (snoc (snoc roundRows divider) totalScore) buttonRow
          ]
        ]
      where 
        roundRows = map roundSlot $ zip (0..length state) state
        roundSlot (Tuple i r) = HH.slot i (Round.ui r) unit (HE.input $ HandleRound i)
        blankCol = HH.div [HP.classes [HH.ClassName "column"]] []
        divider = HH.div [HP.classes [HH.ClassName "ui divider"]] []
        totalScore = HH.h1
          [ HP.classes [HH.ClassName "ui row centered header"] ]
          [HH.text $ show (scoreGame state)]
        buttonRow = HH.div
          [ HP.classes [HH.ClassName "row"]]
          [ HH.button
            [ HP.classes [HH.ClassName "ui primary fluid button"]
            , HE.onClick (HE.input_ Save)
            ]
            [ HH.text "Save"]
          ]
    eval :: Query ~> H.ParentDSL State Query Round.Query Slot Message (Aff (now :: NOW, dom :: DOM | e))
    eval (HandleRound i (Round.RoundChange r) next) = do
      state <- H.get
      H.put $ set (ix i) r state
      pure next
    eval (Save next) = do
        state <- H.get
        d <- liftEff nowDateTime
        let gs = GameSave {playedOn:d, game:state}
        liftEff $ saveGame gs
        H.raise $ GameSaved gs
        pure next
