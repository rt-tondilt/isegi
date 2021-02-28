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

-- The main types. 

data Place = Place {loc::List Int, document::Document}

data Msg = EventMsg {event::Event, revloc:: List Int}
         | Remount

data WidgetState r 
    = Done r
    | Yield (YieldedWidget r) 

type Next r = Msg -> Widget r

type YieldedWidget r = {node::Node, next::Next r}

newtype Widget r = Widget (Place -> Effect (WidgetState r))

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
    pure x = Widget \p -> pure $ Done x

instance bindGen :: Bind Widget where
    -- w1 :: Widget (Place -> Effect (WidgetState a))
    -- f :: a -> Widget (Place -> Effect (WidgetState b))
    bind (Widget w) f = Widget \p -> do
        g1 <- w p -- run the effect
        case g1 of 
            Done r -> unWidget (f r) p
            Yield yielded -> pure $ Yield {
                node: yielded.node, 
                next: \msg -> yielded.next msg >>= f
            }

instance mGen :: Monad Widget

instance monadEffectWidget :: MonadEffect Widget where
    liftEffect e = Widget \p -> do
        v <- e 
        pure $ Done v


---------------

unWidget :: forall r. Widget r -> Place -> Effect (WidgetState r)
unWidget (Widget r) = r

yieldToRight (Done r) = Left r
yieldToRight (Yield yielded) = Right yielded




text :: forall a. String -> Widget a
text t = Widget \(Place place) -> do
    this <- Text.toNode <$> createTextNode t place.document
    pure $ Yield {
        node: this,
        next: \e -> text t
    }
{-
--elementOfGen :: forall a. String -> Array (Widget a) -> Widget a

elementState :: forall a. Element -> Array (Msg -> Widget a) -> Msg -> Widget a
elementState this children msg = Widget \(Place place) -> do
    case msg of
        EventMsg {event, revloc} -> case revloc of
            n : tail -> case children !! n of
                Just child -> do
                    state <- unWidget (child $ EventMsg {event, revloc:tail}) $ Place {loc: n : place.loc, parent: this, document: place.document}
                    pure $ case state of
                        Done r -> Done r
                        Yield next -> ?kkk -- elementState this (updateAt n next children <|> Just children)
                Nothing -> ?k
            Nil -> ?m
        Remount -> do
            _ <- appendChild (Element.toNode this) (Element.toNode place.parent)
            ?k 
d= 3         -}   


--placeChildWidgets :: forall a. Place -> Array (Widget a) -> Effect (Either a (Array (YieldedWidget a)))

placeChildWidgets :: forall a. 
    Place -> Array (Widget a) -> Effect Element -> (Element -> Array (Next a) -> Next a) -> Effect (WidgetState a)
placeChildWidgets (Place place) childWidgets thisMaker nextThis = do

    let numberedChildWidgets = mapWithIndex (\i child -> {i, child}) childWidgets

    widgetStates <- for numberedChildWidgets \{i, child} -> 
        unWidget child $ Place {loc: i:place.loc, document: place.document}

    let eitherGens = sequence $ map yieldToRight widgetStates

    case eitherGens of
        Left r -> pure $ Done r
        Right childYields -> do
            this <-  thisMaker
            for_ childYields \{node} -> appendChild node $ Element.toNode this
            pure $ Yield $ {
                node: Element.toNode this,
                next: nextThis this $ map (_.next) childYields
            } 




element :: forall a. String -> Array (Widget a) -> Widget a
element tag widgets = Widget \(Place place) -> do
    
    -- set up this node
    let thisMaker = createElement tag place.document
   
    let nextThis that childNexts msg = Widget \(Place newPlace) -> case msg of
            EventMsg {event, revloc} -> case revloc of
                n : tail -> case childNexts !! n of
                    Just next -> ?us
                    Nothing -> do
                        error "Bad index"
                        pure $ Yield {
                            node: that,
                            next: nextThis that childNexts 
                        }
                    --let advanceRight i next = ?o
                    --sequence $ mapWithIndex advanceRight childNexts
                Nil -> ?m
            Remount -> do
                let newWidgets = map (\next -> next msg) childNexts
                placeChildWidgets (Place newPlace) newWidgets thisMaker ?k --nextThis


    placeChildWidgets (Place place) widgets thisMaker ?kk -- nextThis 
                --_ <- appendChild (Element.toNode this) (Element.toNode newPlace.parent)
                --pure $ Yield $ nextThis childNexts

    {-
    case eitherGens of
        Left r -> pure $ Done r
        Right childYields -> do
            this <- Element.toNode <$> createElement tag place.document
            for_ childYields \{node} -> appendChild node this
            pure $ Yield $ {
                node: this,
                next: nextThis childYields
            -} 
            
            -- nextThis childNexts
        {-\{event, revloc} -> case revloc of
            (n:tail) -> let
                forward = \next -> next {event, revloc:tail}
                newGen = map forward nexts !! n
                    in ?k
            Nil -> ?x -}
{-
element :: forall a. String -> Array (Widget a) -> Widget a
element tag childWidgets = Widget \(Place place) -> do
    -- set up this node
    this <- createElement tag place.document
    let parent = Element.toNode place.parent
    _ <- appendChild (Element.toNode this) parent

    let numberedChildWidgets = mapWithIndex (\i child -> {i, child}) childWidgets

    -- Try to put all the Widgets inside this.
    gens <- for numberedChildWidgets \{i, child} -> 
        unWidget child $ Place {loc: i:place.loc, parent: this, document: place.document}

    let eitherGens = sequence $ map yieldToRight gens
    ?t
    case eitherGens of
        Left r -> pure $ Done r
        Right nexts -> pure $ Yield \{event, revloc} -> case revloc of
            (n:tail) -> let
                forward = \next -> next {event, revloc:tail}
                newGen = map forward nexts !! n
                    in ?k
            Nil -> ?x -- should not happen
        

    --pure $ Yield \{event, revloc} -> case revloc of
    --    (n:tail) -> ?k
     --   Nil -> ?x -- should not happen

{-
button :: forall a. String -> Widget unit
button t = Widget \(Place place) -> do
    -- TODO ??? clear parent node
    this <- createElement "button" place.document
    let parent = Element.toNode place.parent
    _ <- appendChild (Text.toNode this) parent
    pure $ Yield \e -> text t
    
-}
{-
button :: Widget Event
button = Widget $ pure $ Yield \x -> (pure x :: Widget Event)

hello :: Widget Int
hello = do
    liftEffect $ log "tere1"
    n <- liftEffect $ log "tere222"
    _ <- button
    t <-button
    b <- Widget do
        log "tere4"
        pure $ Done "unit"
    pure t
-}




main :: Effect Unit
main = e
    where
    
    e = error $ show $ 2 + x
    x = 2 * 2
     
            

    
