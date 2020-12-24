module MainU where

import Data.Tuple
import Prelude

import Control.Alt (class Alt)
import Data.List (List(..), (:))
import Effect (Effect)
import Effect.Console (log)


type Event = {relloc::List Int, msg::String}

data HTML = HTML {tag::String, children::Array HTML, loc::List Int}


--type HTMLMaker = List Int -> HTML

data Gen r 
    = Done r
    | Yield (Widget r)


newtype Widget r = Widget (Event -> Effect (Gen r))

unWidget (Widget r) = r 
{-
instance functorGen :: Functor Widget where
    map = liftM1

instance applyGen :: Apply Widget where
    apply = ap

instance applicativeGen :: Applicative Widget where
    pure = Done

instance bindGen :: Bind Widget where
    -- bind :: Effect (Gen a) -> (a -> Effect (Gen b)) -> Effect (Gen r)
    bind (Widget w) f = Widget \e -> do
        g1 <- w e
        case g1 of
            Done r -> pure $ Yield $ f r
            a@(Yield n) -> pure $ ?y
    --bind (Done a) f = f a
    --bind (Yield next) f = Yield (\x -> next x >>= f)

instance mGen :: Monad Widget
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



main :: Effect Unit
main = do
    log $ "tere" 
            

    
