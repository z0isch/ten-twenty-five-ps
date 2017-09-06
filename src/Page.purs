module Page where

import Prelude

import Control.Coroutine (Consumer, consumer)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Console (CONSOLE, logShow)
import Control.Monad.Eff.Now (NOW)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Window (localStorage)
import Data.Either.Nested (Either3)
import Data.Functor.Coproduct.Nested (Coproduct3)
import Data.Maybe (Maybe(..))
import Halogen (liftEff, modify)
import Halogen as H
import Halogen.Component.ChildPath (cp1, cp2, cp3)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Routes (Routes(..))
import Routes.History as History
import Routes.Home as Home
import Routes.PlayGame as PlayGame
import TenTwentyFive.Types (GameSave(..))

type State = {currentPage :: Routes, history :: Array GameSave}

data Query a
  = Goto Routes a
  | HandlePlayGame PlayGame.Message a
  | SetHistory (Array GameSave) a

type Input = Unit

data Message = RouteTo String

type ChildQuery = Coproduct3 Home.Query PlayGame.Query History.Query

type ChildSlot = Either3 Unit Unit Unit

ui :: forall e. H.Component HH.HTML Query Input Message (Aff (now :: NOW , dom :: DOM | e))
ui = H.parentComponent 
  { initialState: const {currentPage: Home, history: []}
  , render
  , eval
  , receiver: const Nothing
  }
  where
    render :: State -> H.ParentHTML Query ChildQuery ChildSlot (Aff (now :: NOW, dom :: DOM | e))
    render {currentPage:Home} = HH.div_
      [ HH.slot' cp1 unit Home.ui unit absurd
      ]
    render {currentPage:PlayGame} = HH.div_
      [ HH.slot' cp2 unit PlayGame.ui unit (HE.input HandlePlayGame)
      ]
    render {currentPage:History, history: games} = HH.div_
        [ HH.slot' cp3 unit History.ui games absurd
        ]
    eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Message (Aff (now :: NOW, dom :: DOM | e))
    eval (Goto a next) = do
      modify (_{ currentPage = a })
      pure next
    eval (HandlePlayGame (PlayGame.GameSaved _) next) = do
      H.raise $ RouteTo "/history"
      pure next
    eval (SetHistory h next) = do
      modify (_{ history = h })
      pure next

routeConsumer :: forall a eff. (Query Unit -> Aff (avar :: AVAR | eff) a)
  ->  Consumer Routes (Aff (avar :: AVAR | eff)) Unit
routeConsumer q = consumer \event -> do
  _ <- q $ H.action $ Goto event
  pure Nothing

savedGamesConsumer :: forall a eff. (Query Unit -> Aff (avar :: AVAR | eff) a)
  ->  Consumer (Array GameSave) (Aff (avar :: AVAR | eff)) Unit
savedGamesConsumer q = consumer \games -> do 
    _ <- q $ H.action $ SetHistory games
    pure Nothing


















