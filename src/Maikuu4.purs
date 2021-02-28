module Maikuu4 where


import Control.Alt
import Data.Maybe
import Data.Tuple
import Prelude

import Data.Array (mapWithIndex, updateAt, (!!))
import Data.Array.ST.Iterator (next)
import Data.Either (Either(..))
import Data.List (List(..), foldl, (:))
import Data.Traversable (for, for_, sequence)
import Effect (Effect)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (error, log)
import Web.DOM (Document, Element, Node)
import Web.DOM.Document (createElement, createTextNode)
import Web.DOM.Element as Element
import Web.DOM.Node (appendChild)
import Web.DOM.Node as Node
import Web.DOM.Text as Text
import Web.Event.Event (Event)


newtype Place = Place {loc::List Int, document::Document}

newtype Msg = Msg {event::Event, revloc:: List Int}

class Widget s r | s -> r where
    info :: s -> r
    put :: s -> Place -> Effect {node :: Node, next :: Msg -> s}


instance name :: Widget (Array (Widget a)) (Array a) where
  