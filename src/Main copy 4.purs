module MainN where

import Data.Tuple
import Prelude

import Control.Alt (class Alt)
import Data.List (List(..), (:))
import Effect (Effect)
import Effect.Console (log)


type Event = {relloc::List Int, msg::String}

data HTML = HTML {tag::String, children::Array HTML, loc::List Int}

type Next r = Event -> Gen r

--type HTMLMaker = List Int -> HTML

data Gen r 
    = Done r
    | Yield (Event -> Gen r)

instance functorGen :: Functor Gen where
    map = liftM1

instance applyGen :: Apply Gen where
    apply = ap

instance applicativeGen :: Applicative Gen where
    pure = Done

instance bindGen :: Bind Gen where
    bind (Done a) f = f a
    bind (Yield next) f = Yield (\x -> next x >>= f)

instance mGen :: Monad Gen

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
            

    
