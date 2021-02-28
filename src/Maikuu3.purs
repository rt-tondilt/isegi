module Maikuu3 where


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

data Widget r = Widget (Effect ({info :: Maybe r, put :: Put r}))

type Exhibit r = {nodes :: Array Node, next :: Msg -> Widget r}

newtype Put r = Put (Place -> Effect (Exhibit r))




instance functorGen :: Functor Widget where
    map = liftM1

instance applyGen :: Apply Widget where
    apply = ap

instance applicativeGen :: Applicative Widget where
    pure x = Widget $ pure {info: Just x, put:Put \place -> pure {nodes: [], next: \msg -> pure x}}

instance bindGen :: Bind Widget where
    -- w1 :: Widget (Place -> Effect (WidgetState a))
    -- f :: a -> Widget (Place -> Effect (WidgetState b))
    bind (Widget widgetEffect) f = Widget do
        {info, put} <- widgetEffect
        let Put put = put
        case info of
            Just r -> let Widget new = f r in new
            Nothing -> pure $ {
                info: Nothing,
                put : Put $ \place -> do
                    {nodes, next} <- put place
                    pure {
                        nodes,
                        next: \msg -> next msg >>= f
                    }
            }

instance mGen :: Monad (Widget)


runAll :: forall r. Array (Widget r) -> Effect (Array ({info:: Maybe r, put:: Put r}))
runAll widgets = for widgets \(Widget eff) -> eff

placeAll :: forall r. Place -> Array (Put r) ->Effect (Array (Exhibit r))
placeAll (Place {loc, document}) puts = 
    sequence $ mapWithIndex (\i (Put put) -> put (Place {loc:i:loc, document})) puts

and :: forall o r. Array (Widget r) -> Widget (Array ({info::r, put::Put r}))
and widgets = Widget $ do
    rwgs  <- runAll widgets
    let resultInfo = for rwgs \{info, put} -> case info of
            Just i ->  Just {info: i, put:put}
            Nothing -> Nothing
    let puts = map (\{info, put: put} -> put) rwgs
    let firstResult = pure $ {
        info: resultInfo,
        put: Put \place -> do
            exhibits <- placeAll place puts
            pure {
                nodes: join $ map (\{nodes, next} -> nodes) exhibits,
                next: \msg -> Widget $ do
                    let Msg {event, revloc} = msg
                    case revloc of
                        index:more -> case exhibits!!index of
                            Just {nodes, next} -> do
                                let Widget newWidgetEffect = next $ Msg {event, revloc:more}
                                {info, put} <- newWidgetEffect

                                ?k
                            Nothing -> do
                                error $ "Widget and at " <> show loc <> 
                                    " recieived invalid index" <> show index <> 
                                    " : " <> show more <> "."
                                firstResult
                        Nil -> do
                            error $ "Widget and at " <> show loc <> 
                                    " recieived event."
                            firstResult
            }
    }
    firstResult
    
