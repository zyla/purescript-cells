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

mapCell :: forall f. Functor f => f Int -> f Int
mapCell = map (add 1) <<< map (add 1) <<< map (add 1)

main :: forall e. Eff (console :: CONSOLE, st :: ST RealWorld | e) Unit
main = do
  let
    benchUpdateRead
      :: forall f
       . Functor f
      => (forall a. a -> Effect { cell :: f a, update :: a -> Effect Unit })
      -> (forall a. f a -> Effect a)
      -> Effect Unit
    benchUpdateRead new read = do
      root <- new 0
      let cell = mapCell root.cell
      foreach_ bigArray $ mkEffFn1 \x -> do
        root.update x
        void (read cell)

  runBench do

    fnEff "Cell0 map update+read" $
      benchUpdateRead Cell0.new Cell0.read

    fnEff "Cell1 map update+read" $
      benchUpdateRead Cell1.new Cell1.read

  let
    benchUpdate
      :: forall f
       . Functor f
      => (forall a. a -> Effect { cell :: f a, update :: a -> Effect Unit })
      -> (forall a. f a -> Effect a)
      -> Effect Unit
    benchUpdate new read = do
      root <- new 0
      let cell = mapCell root.cell
      foreach_ bigArray $ mkEffFn1 \x -> do
        root.update x
        void (read cell)

  runBench do

    fnEff "Cell0 map update" $
      benchUpdate Cell0.new Cell0.read

    fnEff "Cell1 map update" $
      benchUpdate Cell1.new Cell1.read
