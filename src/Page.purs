module Page where

import Prelude

import Control.Coroutine (Consumer, consumer)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff.Now (NOW)
import DOM (DOM)
import Data.Either.Nested (Either2)
import Data.Functor.Coproduct.Nested (Coproduct2)
import Data.Maybe (Maybe(..))
import Halogen (modify)
import Halogen as H
import Halogen.Component.ChildPath (cp1, cp2)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Routes (Routes(..))
import Routes.PlayGame as PlayGame
import Routes.Home as Home

type State = {currentPage :: Routes}

data Query a
  = Goto Routes a
  | HandlePlayGame PlayGame.Message a

type Input = Unit

data Message = RouteTo String

type ChildQuery = Coproduct2 Home.Query PlayGame.Query

type ChildSlot = Either2 Unit Unit

ui :: forall e. H.Component HH.HTML Query Input Message (Aff (now :: NOW , dom :: DOM | e))
ui = H.parentComponent 
  { initialState: const {currentPage: Home}
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
    eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Message (Aff (now :: NOW, dom :: DOM | e))
    eval (Goto a next) = do
      modify (_{ currentPage = a })
      pure next
    eval (HandlePlayGame (PlayGame.GameSaved _) next) = do
      H.raise $ RouteTo "/"
      pure next

routeConsumer :: forall a eff. (Query Unit -> Aff (avar :: AVAR | eff) a)
  ->  Consumer Routes (Aff (avar :: AVAR | eff)) Unit
routeConsumer q = consumer \event -> do
  _ <- q $ H.action $ Goto event
  pure Nothing








