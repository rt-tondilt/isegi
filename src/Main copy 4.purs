module MainN where

import Data.Tuple
import Prelude

import Control.Alt (class Alt)
import Data.Array (mapWithIndex, modifyAt, (!!))
import Data.Either (Either(..))
import Data.List (List(..), foldl, (:))
import Data.Traversable (for, sequence)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log)
import Web.DOM (Document, Element, Node)
import Web.DOM.Document (createElement, createTextNode)
import Web.DOM.Element as Element
import Web.DOM.Node (appendChild)
import Web.DOM.Node as Node
import Web.DOM.Text as Text
import Web.Event.Event (Event)


data Place = Place {loc::List Int, parent::Element, document::Document}

type Message = {event::Event, revloc:: List Int}

-- data HTML = HTML {tag::String, children::Array HTML, loc::List Int}



--type HTMLMaker = List Int -> HTML

data Gen r 
    = Done r
    | Yield (Message -> Widget r)

newtype Widget r = Widget (Place -> Effect (Gen r))

unWidget :: forall r. Widget r -> Place -> Effect (Gen r)
unWidget (Widget r) = r

yieldToRight (Done r) = Left r
yieldToRight (Yield next) = Right next

instance functorGen :: Functor Widget where
    map = liftM1

instance applyGen :: Apply Widget where
    apply = ap

instance applicativeGen :: Applicative Widget where
    pure x = Widget \p -> pure $ Done x

instance bindGen :: Bind Widget where
    bind (Widget w) f = Widget \p -> do
        g1 <- w p -- run the effect
        case g1 of 
            Done r -> unWidget (f r) p
            Yield next -> pure $ Yield $ \x -> next x >>= f
    --bind (Done a) f = f a
    --bind (Yield next) f = Yield (\x -> next x >>= f)

instance mGen :: Monad Widget

instance monadEffectWidget :: MonadEffect Widget where
    liftEffect e = Widget \p -> do
        v <- e 
        pure $ Done v

text :: forall a. String -> Widget a
text t = Widget \(Place place) -> do
    -- TODO ??? clear parent node
    this <- createTextNode t place.document
    let parent = Element.toNode place.parent
    _ <- appendChild (Text.toNode this) parent
    pure $ Yield \e -> text t


--elementOfGen :: forall a. String -> Array (Widget a) -> Widget a
{-
element :: forall a. String -> Array (Widget a) -> Widget a
element tag children = Widget \(Place place) -> do
    this <- createElement tag place.document
    let parent = Element.toNode place.parent
    _ <- appendChild (Element.toNode this) parent
    let numberedChildren = mapWithIndex (\i child -> {i, child}) children

    -- Try to put all the Widgets inside this.
    gens <- for numberedChildren \{i, child} -> 
        unWidget child $ Place {loc: i:place.loc, parent: this, document: place.document}

    let eitherGens = sequence $ map yieldToRight gens
    
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
-}
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


{-
pair :: forall r. Gen r -> Gen r -> Gen r
pair (Done r) _ = Done r
pair _ (Done r) = Done r
pair a@(Yield aHtml aNext) b@(Yield bHtml bNext) = Yield html next
    where
        html loc = HTML {tag:"div", children: [aHtml (0:loc), bHtml (1:loc)], loc}
        next {relloc, msg} = case relloc of
            (0:tail) -> pair (aNext {relloc:tail, msg}) b
            (1:tail) -> pair a (bNext {relloc:tail, msg})
            (_:_) -> pair a b -- should not happen
            Nil -> pair a b -- should not happen
-}
{-
run :: forall a. Widget a -> Effect a
run w = do
    m <- unWidget w
    case m of
        Done r -> do
            log "return"
            pure r
        Yield n -> do
            log "yield"
            run $ n 777 
-}

main :: Effect Unit
main = do
    log $ "show"
     
            

    
