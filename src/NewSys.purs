module NewSys where

import Data.Tuple
import Prelude

import Control.Alt (class Alt)
import Effect (Effect)
import Effect.Console (log)

data Yielded i o r = Yielded o (i -> Gen i o r)

data Gen i o r 
    = Done r
    | Yield o (i -> Gen i o r)


instance functorGen :: Functor (Gen i o) where
    map = liftM1

instance applyGen :: Apply (Gen i o) where
    apply = ap

instance applicativeGen :: Applicative (Gen i o) where
    pure = Done

instance bindGen :: Bind (Gen i o) where
    bind (Done a) f = f a
    bind (Yield o k) f = Yield o \x -> k x >>= f

instance mGen :: Monad (Gen i o)

yield :: forall o i. o -> Gen i o i
yield o = Yield o Done

mapInput :: forall i1 i2 o r. (i1 -> i2) -> Gen i2 o r -> Gen i1 o r
mapInput f (Done r) = Done r
mapInput f (Yield o k) = Yield o \x -> mapInput f $ k $ f x

mapOutput :: forall i o1 o2 r. (o1 -> o2) -> Gen i o1 r -> Gen i o2 r
mapOutput f (Done r) = Done r
mapOutput f (Yield o k) = Yield (f o) \x -> mapOutput f $ k x

--merge :: forall i1 i2 i3 o1 o2 o3. ()
merge :: forall i1 i2 i3 o1 o2 o3 r . 
    (Yielded i1 o1 r -> Yielded i2 o2 r -> Yielded i3 o3 r) 
      -> Gen i1 o1 r ->     Gen i2 o2 r ->     Gen i3 o3 r
merge f (Done r) _ = Done r
merge f _ (Done r) = Done r
merge f (Yield o1 k1) (Yield o2 k2) = Yield o3 k3
    where Yielded o3 k3 = f (Yielded o1 k1) (Yielded o2 k2) 

type HTML = String

data Output a = Output HTML a

type Widget i o r = Gen 




data Never

back :: forall a b. a -> Gen a a b
back d = do
  r <- yield d
  back r

{-nn = do
    x <- yield "40"
    y <- mapOutput (_ == "tere") $ yield "ff"
    yield if y then x else "100"-}




main :: Effect Unit
main = do
    log $ "tere" <> o1 <> o2
        where
            def g t = case g t of
                Yield o k -> Tuple o k
                Done _ -> Tuple "-10" Done
            (Tuple o1 k1) = def back "+0"
            (Tuple o2 k2) = def k1 "+10"
            

    
