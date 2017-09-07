module Routes where

import Prelude

import Control.Alt ((<|>))
import Control.Coroutine (Producer)
import Control.Coroutine.Aff (produce)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Data.Either (Either(..))
import Data.Generic (class Generic, gShow)
import Routing (matches)
import Routing.Match (Match)
import Routing.Match.Class (lit)

data Routes 
  = Home
  | PlayGame
  | Stats
  | About
derive instance genericRoutes :: Generic Routes
instance showRoutes :: Show Routes where
  show = gShow

homeSlash :: Match Unit
homeSlash = lit ""

playGame :: Match Routes
playGame = PlayGame <$ (homeSlash *> lit "playGame")

stats :: Match Routes
stats = Stats <$ (homeSlash *> lit "stats")

about :: Match Routes
about = About <$ (homeSlash *> lit "about")

home :: Match Routes
home = Home <$ homeSlash

routing :: Match Routes
routing = 
  playGame <|>
  stats <|>
  about <|>
  home

routeProducer :: forall eff. Producer Routes (Aff (avar :: AVAR | eff)) Unit
routeProducer = produce \emit -> matches routing \_ r -> emit $ Left r