module Main where

import Prelude

import Benchmark (fnEff, runBench)
import Cell0 as Cell0
import Cell1 as Cell1
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Uncurried (EffFn1, mkEffFn1)
import Control.Monad.ST (ST)
import Data.Array as Array
import Effect (RealWorld, Effect)

foreign import foreach_ :: forall e a. Array a -> EffFn1 e a Unit -> Eff e Unit

bigArray :: Array Int
bigArray = Array.range 0 10000

main :: forall e. Eff (console :: CONSOLE, st :: ST RealWorld | e) Unit
main = do
  let
    benchUpdateRead
      :: forall f
       . Functor f
      => (forall a. a -> Effect { cell :: f a, update :: a -> Effect Unit })
      -> (forall a. f a -> Effect a)
      -> (f Int -> f Int)
      -> Effect Unit
    benchUpdateRead new read transform = do
      root <- new 0
      let cell = transform root.cell
      foreach_ bigArray $ mkEffFn1 \x -> do
        root.update x
        void (read cell)

  let
    benchRead
      :: forall f
       . Functor f
      => (forall a. a -> Effect { cell :: f a, update :: a -> Effect Unit })
      -> (forall a. f a -> Effect a)
      -> (f Int -> f Int)
      -> Effect Unit
    benchRead new read transform = do
      root <- new 0
      let cell = transform root.cell
      foreach_ bigArray $ mkEffFn1 \x -> do
        void (read cell)

  runBench do

    fnEff "Cell0 exponential update+read" $
      benchUpdateRead Cell0.new Cell0.read graphExponential

    fnEff "Cell1 exponential update+read" $
      benchUpdateRead Cell1.new Cell1.read graphExponential

  runBench do

    fnEff "Cell0 exponential read" $
      benchRead Cell0.new Cell0.read graphExponential

    fnEff "Cell1 exponential read" $
      benchRead Cell1.new Cell1.read graphExponential

  runBench do

    fnEff "Cell0 map update+read" $
      benchUpdateRead Cell0.new Cell0.read graphMap

    fnEff "Cell1 map update+read" $
      benchUpdateRead Cell1.new Cell1.read graphMap

  runBench do

    fnEff "Cell0 map read" $
      benchRead Cell0.new Cell0.read graphMap

    fnEff "Cell1 map read" $
      benchRead Cell1.new Cell1.read graphMap

expensiveMath :: Int -> Int
expensiveMath x = 100000 `div` x

graphMap :: forall f. Functor f => f Int -> f Int
graphMap = map expensiveMath <<< map expensiveMath <<< map expensiveMath

graphExponential :: forall f. Applicative f => f Int -> f Int
graphExponential = apSelf <<< apSelf <<< apSelf
  where
    apSelf x = (\a b -> expensiveMath (a + b)) <$> x <*> x
