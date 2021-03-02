module Maikuu3 where


import Control.Alt
import Data.Maybe
import Data.Tuple
import Prelude

import Data.Array (mapWithIndex, updateAt, (!!), foldl)
import Data.Array as Array
import Data.Either (Either(..))
import Data.List (List(..), (:))
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


data Widget r = Widget (Effect (WidgetResult r))

type WidgetResult r = {info :: Maybe r, put :: Put r}

newtype Put r = Put (Place -> Effect (Exhibit r))

type Exhibit r = {nodes :: Array Node, next :: Msg -> Widget r}



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

instance monadEffectWidget :: MonadEffect Widget where
    liftEffect e = Widget do
        v <- e 
        pure $  {info: Just v, put:Put \place -> pure {nodes: [], next: \msg -> pure v}}

runAll :: forall r. Array (Widget r) -> Effect (Array ({info:: Maybe r, put:: Put r}))
runAll widgets = for widgets \(Widget eff) -> eff

placeAll :: forall r. Place -> Array (Put r) ->Effect (Array (Exhibit r))
placeAll (Place {loc, document}) puts = 
    sequence $ mapWithIndex (\i (Put put) -> put (Place {loc:i:loc, document})) puts

--makeWidgetResult :: forall i r. Maybe i -> (Array (WidgetResult r) -> WidgetResult i) -> WidgetResult i 
makeWidgetResult :: forall r1 r2.
  Maybe r1 -> (Array (WidgetResult r2) -> WidgetResult r1) -> Array (WidgetResult r2) -> WidgetResult r1
makeWidgetResult resultInfo helper rwgs =
    let firstResult = {
        info: resultInfo,
        put: Put \place -> do
            let puts = map (\{info, put: put} -> put) rwgs
            exhibits <- placeAll place puts
            let Place {loc} = place
            let next =  \msg -> Widget $ do
                    let Msg {event, revloc} = msg
                    case revloc of
                        index:more -> case exhibits!!index of
                            Just {nodes, next} -> do
                                let Widget newWidgetEffect = next $ Msg {event, revloc:more}
                                newWidgetResult <- newWidgetEffect
                                let newRwgs = fromMaybe rwgs $ Array.updateAt index newWidgetResult rwgs
                                pure $ helper newRwgs
                                
                            Nothing -> do
                                error $ "array widget at " <> show loc <> 
                                    " recieived invalid index" <> show index <> 
                                    " : " <> show more <> "."
                                pure $ firstResult
                        Nil -> do
                            error $ "array widget at " <> show loc <> 
                                    " recieived event."
                            pure $ firstResult
            pure {
                nodes: join $ map (_.nodes) exhibits,
                next
            }
    }
    in firstResult

andHelper :: forall r. Array (WidgetResult r) -> WidgetResult (Array {info::r, widget::Widget r})
andHelper rwgs =
    let 
        resultInfo :: Maybe (Array {info:: r, widget:: Widget r})
        resultInfo = for rwgs \{info, put} -> case info of
            Just i ->  Just {info: i, widget: Widget $ pure $ {info, put}}
            Nothing -> Nothing
        
    in makeWidgetResult resultInfo andHelper rwgs

and :: forall r. Array (Widget r) -> Widget (Array {info::r, widget::Widget r})
and widgets = Widget do
    rwgs  <- runAll widgets
    pure $ andHelper rwgs


orHelper :: forall r. Array (WidgetResult r) -> WidgetResult {info::r, widgets:: Array (Widget r)}
orHelper rwgs =
    let 
        resultInfo :: Maybe {info::r, widgets:: Array (Widget r)}
        resultInfo = foldl f Nothing rwgs
            where
                f (Just x) e = Just x
                f Nothing {info: Just i} = Just {info: i, widgets: map (\wr -> Widget $ pure wr) rwgs}
                f Nothing {info: Nothing} = Nothing

    in makeWidgetResult resultInfo orHelper rwgs

or :: forall r. Array (Widget r) -> Widget {info::r, widgets:: Array (Widget r)}
or widgets = Widget do
    rwgs  <- runAll widgets
    pure $ orHelper rwgs

text :: forall a. String -> Widget a
text t = Widget $ pure {
    info: Nothing,
    put: Put $ \(Place {loc, document}) -> do
        this <- Text.toNode <$> createTextNode t document
        pure $ {
            nodes: [this],
            next: \msg -> do
                liftEffect $ error $ "Text widget" <> show t <> " at " <> show loc <> " got a message."
                text t
        }
}
    

    
el :: forall r. String -> Maybe r -> Widget r -> Widget r
el tag onclick (Widget widgetEffect) = Widget do
    {info, put: (Put childPut)} <- widgetEffect
    let fin = {
        info,
        put: Put \(Place {loc, document}) -> do
            {nodes, next} <- childPut $ Place {loc: 0:loc, document} 
            this <- createElement tag document
            let thisNode = Element.toNode this
            for_ nodes \childNode -> appendChild childNode thisNode
            pure {
                nodes: [thisNode],
                next: \(Msg {event, revloc}) -> Widget
                    case revloc of
                        0:more ->  do
                            let newWidget = next $ Msg {event, revloc:more}
                            let Widget newef = el tag onclick newWidget
                            newef
                        index:more -> do
                            error $ "element widget at " <> show loc <> 
                                    " recieived invalid index" <> show index <> 
                                    " : " <> show more <> "."
                            pure $ fin
                        Nil -> do
                            
                            error $ "array widget at " <> show loc <> 
                                    " recieived event."
                            pure $ fin
                    
            }
    }
    pure fin    
 