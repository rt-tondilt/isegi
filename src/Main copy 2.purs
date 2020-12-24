module MainError where

import Data.Tuple
import Prelude

import Control.Alt (class Alt)
import Effect (Effect)
import Effect.Console (log)

type Event = String

--type HTML = {tag::String, children::Array HTML, next::(Event -> Gen Int)}

{-data Gen r 
    = Done r
    | Yield HTML

instance functorGen :: Functor Gen where
    map = liftM1

instance applyGen :: Apply Gen where
    apply = ap

instance applicativeGen :: Applicative Gen where
    pure = Done

instance bindGen :: Bind Gen where
    bind (Done a) f = f a
    bind (Yield html) f = Yield html {next = (\x -> html.next x >>= f)}

instance mGen :: Monad Gen-}

main :: Effect Unit
main = do
    log $ "tere" 
            

    
