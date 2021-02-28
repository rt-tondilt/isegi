module Maikuu where


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

data WidgetResult o r 
    = Done r
    | Yield (RWG o r)
    

newtype Widget o r = Widget (Effect (WidgetResult o r))

data RWG o r = RWG {info :: o, put :: Place -> Effect {node :: Node, next :: Msg -> Widget o r}}



instance functorGen :: Functor (Widget o) where
    map = liftM1

instance applyGen :: Apply (Widget o) where
    apply = ap

instance applicativeGen :: Applicative (Widget o) where
    pure x = Widget $ pure $ Done x

instance bindGen :: Bind (Widget o) where
    -- w1 :: Widget (Place -> Effect (WidgetState a))
    -- f :: a -> Widget (Place -> Effect (WidgetState b))
    bind (Widget widgetEffect) f = Widget do
        w <- widgetEffect
        case w of
            Done r -> let Widget new = f r in new
            Yield (RWG {info, put}) -> pure $ Yield $ RWG {
                info: info,
                put : \pl -> do
                    {node, next} <- put pl
                    pure $ {
                        node: node,
                        next: \msg -> next msg >>= f
                    }
            } 

instance mGen :: Monad (Widget o)


orr :: forall o r. Array (Widget o r) -> Widget (Array (RWG o r)) r