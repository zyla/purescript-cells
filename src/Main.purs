module Main where

import Prelude

import Benchmark (fnEff, runBench)
import Benchmark.Suite.Monad (SuiteT)
import Cell0 as Cell0
import Cell00 as Cell00
import Cell1 as Cell1
import Cell2 as Cell2
import Cell3 as Cell3
import Cell4 as Cell4
import Cell5 as Cell5
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Uncurried (EffFn1, EffFn2, mkEffFn1, runEffFn1, runEffFn2)
import Control.Monad.ST (ST)
import Data.Array as Array
import Data.Foldable (for_)
import Effect (RealWorld, E)
import IsCell (class IsCell, FProxy(FProxy), Root, implName, newP, read, readRoot, update)

foreign import foreach_ :: forall e a. Array a -> EffFn1 e a Unit -> Eff e Unit

newtype SomeCellImpl = SomeCellImpl (forall r. (forall f. IsCell f => FProxy f -> r) -> r)

withImpl :: forall r. SomeCellImpl -> (forall f. IsCell f => FProxy f -> r) -> r
withImpl (SomeCellImpl impl) fn = impl fn

mkImpl :: forall f. IsCell f => FProxy f -> SomeCellImpl
mkImpl proxy = SomeCellImpl (\fn -> fn proxy)

impls :: Array SomeCellImpl
impls =
  [ mkImpl (FProxy :: FProxy Cell0.Cell)
  , mkImpl (FProxy :: FProxy Cell1.Cell)
  , mkImpl (FProxy :: FProxy Cell00.Cell)
  , mkImpl (FProxy :: FProxy Cell2.Cell)
  , mkImpl (FProxy :: FProxy Cell3.Cell)
  , mkImpl (FProxy :: FProxy Cell4.Cell)
  , mkImpl (FProxy :: FProxy Cell5.Cell)
  ]

graphs :: Array Graph
graphs =
  [ graphFunkia
  , graphMap 0
  , graphMap 3
  , graphMap 10
  , graphExponential 1
  , graphExponential 2
  , graphExponential 3
  , graphExponential 4
  , graphExponential 5
  ]

type Suite = SuiteT RealWorld (st :: ST RealWorld, console :: CONSOLE)
type Test = SomeCellImpl -> Graph -> Suite Unit

tests :: Array Test
tests =
  [ benchUpdateRead
  , benchRead
  ]

benchUpdateRead :: Test
benchUpdateRead impl (Graph graph) = withImpl impl go
  where
    go :: forall f. IsCell f => FProxy f -> Suite Unit
    go (proxy :: FProxy f) = do
      let name = implName proxy <> " " <> graph.name <> " update+read"
      fnEff name do
        root <- newP proxy 0
        let
          cell = graph.construct (readRoot root)
          read' :: EffFn1 E (f Int) Int
          read' = read
          update' :: EffFn2 E (Root f Int) Int Unit
          update' = update
        foreach_ bigArray $ mkEffFn1 \x -> do
          runEffFn2 update' root x
          _ <- runEffFn1 read' cell
          pure unit

benchRead :: Test
benchRead impl (Graph graph) = withImpl impl go
  where
    go :: forall f. IsCell f => FProxy f -> Suite Unit
    go (proxy :: FProxy f) = do
      let name = implName proxy <> " " <> graph.name <> " read"
      fnEff name do
        root <- newP proxy 0
        let
          cell = graph.construct (readRoot root)
          read' :: EffFn1 E (f Int) Int
          read' = read
        foreach_ bigArray $ mkEffFn1 \_ -> do
          _ <- runEffFn1 read' cell
          pure unit

main :: Eff (console :: CONSOLE, st :: ST RealWorld) Unit
main = do

  for_ graphs \graph ->
    for_ tests \test ->
      runBench do
        for_ impls \impl -> do
          test impl graph

newtype Graph = Graph
  { construct :: forall f. Applicative f => f Int -> f Int
  , name :: String
  }

graphFunkia :: Graph
graphFunkia =
  Graph
    { construct: map (_ - 3) <<< map (_ * 2) <<< map (_ + 1)
    , name: "funkia map-map-map"
    }

graphMap :: Int -> Graph
graphMap n =
  Graph
    { construct: fnN n (map expensiveMath)
    , name: show n <> "x map expensiveMath"
    }

graphExponential :: Int -> Graph
graphExponential n =
  Graph
    { construct:
        let apSelf x = (\a b -> expensiveMath (a + b)) <$> x <*> x
        in fnN n apSelf
    , name: show n <> "x expensiveMath (a + a)"
    }

-------------------------------

fnN :: forall a. Int -> (a -> a) -> a -> a
fnN 0 _ = id
fnN n f = f <<< fnN (n - 1) f

expensiveMath :: Int -> Int
expensiveMath x = 100000 `div` x

bigArray :: Array Int
bigArray = Array.range 0 100000
