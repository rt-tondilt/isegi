module Main where

import Data.Tuple
import Prelude

import Control.Alt (class Alt)
import Data.List (List(..), (:))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log)


type Event = Int -- {relloc::List Int, msg::String}

data HTML = HTML {tag::String, children::Array HTML, loc::List Int}

type Next r = Event -> Gen r

--type HTMLMaker = List Int -> HTML

data Gen r 
    = Done r
    | Yield (Event -> Widget r)

newtype Widget r = Widget (Effect (Gen r))

unWidget (Widget r) = r

instance functorGen :: Functor Widget where
    map = liftM1

instance applyGen :: Apply Widget where
    apply = ap

instance applicativeGen :: Applicative Widget where
    pure x = Widget $ pure $ Done x

instance bindGen :: Bind Widget where
    bind (Widget w) f = Widget do
        g1 <- w -- run the effect
        case g1 of 
            Done r -> unWidget (f r)
            Yield next -> pure $ Yield $ \x -> next x >>= f
    --bind (Done a) f = f a
    --bind (Yield next) f = Yield (\x -> next x >>= f)

instance mGen :: Monad Widget

instance monadEffectWidget :: MonadEffect Widget where
    liftEffect e = Widget do
        v <- e
        pure $ Done v

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

main :: Effect Unit
main = do
    r <- run hello
    log $ show r
     
            

    
