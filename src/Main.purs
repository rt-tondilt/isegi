module Main where

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
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (error, log)
import Web.DOM (Document, Element, Node)
import Web.DOM.Document (createElement, createTextNode)
import Web.DOM.Element as Element
import Web.DOM.Node (appendChild)
import Web.DOM.Node as Node
import Web.DOM.Text as Text
import Web.Event.Event (Event)
import Web.HTML.Event.EventTypes (offline)

-- The main types. 

newtype Place = Place {loc::List Int, document::Document}

newtype Msg = Msg {event::Event, revloc:: List Int}

data WidgetState r 
    = Done r
    | Yield (Place -> Effect (PlacedWidget r)) 


newtype PlacedWidget r = PlacedWidget {node:: Node, next :: Msg -> Widget r}

newtype Widget r = Widget (Effect (WidgetState r))

-- WidgetState is a monad.
{-
instance functorWS :: Functor WidgetState where
    map = liftM1

instance applyWS :: Apply WidgetState where
    apply = ap

instance applicativeWS :: Applicative WidgetState where
    pure = Done

instance bindWS :: Bind WidgetState where
    bind (Done a) f = f a
    bind (Yield next) f = Yield \msg -> Widget \p -> do
        nextState <- unWidget (next msg) p
        pure $ nextState >>= f

instance monadWS :: Monad WidgetState

instance monadEffectWS :: MonadEffect WidgetState where
    liftEffect e = Yield \msg -> Widget \p -> do 
        v <- e 
        pure $ Done v


-}

-- Widget is a monad.

instance functorGen :: Functor Widget where
    map = liftM1

instance applyGen :: Apply Widget where
    apply = ap

instance applicativeGen :: Applicative Widget where
    pure x = Widget $ pure $ Done x

instance bindGen :: Bind Widget where
    -- w1 :: Widget (Place -> Effect (WidgetState a))
    -- f :: a -> Widget (Place -> Effect (WidgetState b))
    bind (Widget widgetEffect) f = Widget do
        w <- widgetEffect
        case w of
            Done r -> let Widget new = f r in new
            Yield d -> pure $ Yield \pl -> do
                PlacedWidget n <- d pl
                pure $ PlacedWidget {
                    node: n.node, 
                    next: \msg -> n.next msg >>= f
                }
    {-bind (Widget w) f = Widget \p -> do
        g1 <- w p -- run the effect
        case g1 of 
            Done r -> unWidget (f r) p
            Yield yielded -> pure $ Yield {
                node: yielded.node, 
                next: \msg -> yielded.next msg >>= f
            }
-}
instance mGen :: Monad Widget

instance monadEffectWidget :: MonadEffect Widget where
    liftEffect e = Widget do
        v <- e 
        pure $ Done v

text :: forall a. String -> Widget a
text t = Widget $ pure $ Yield \(Place place) -> do
    this <- Text.toNode <$> createTextNode t place.document
    pure $ PlacedWidget {
        node: this,
        next: \msg -> do
            liftEffect $ error "BAD"
            text t
    }

{-
text :: forall a. String -> Widget a
text t = Widget \(Place place) -> do
    this <- Text.toNode <$> createTextNode t place.document
    pure $ Yield {
        node: this,
        next: \e -> text t
    }
-}

main :: Effect Unit
main = e
    where
    
    e = error $ show $ 2 + x
    x = 2 * 2
     
            

    
