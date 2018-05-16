module Main where

import Prelude

import Benchmark (fnEff, runBench)
import Benchmark.Suite.Monad (SuiteT)
import Cell0 as Cell0
import Cell1 as Cell1
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Uncurried (EffFn1, mkEffFn1)
import Control.Monad.ST (ST)
import Data.Array as Array
import Data.Foldable (for_)
import Effect (RealWorld)
import IsCell (class IsCell, FProxy(FProxy), implName, newP, read)

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
  ]

graphs :: Array Graph
graphs =
  [ graphMap 1
  , graphMap 3
  , graphMap 10
  , graphExponential 1
  , graphExponential 2
  , graphExponential 3
  , graphExponential 4
  , graphExponential 5
  ]

type Test = SomeCellImpl -> Graph -> SuiteT RealWorld (st :: ST RealWorld, console :: CONSOLE) Unit

tests :: Array Test
tests =
  [ benchUpdateRead
  , benchRead
  ]

benchUpdateRead :: Test
benchUpdateRead impl (Graph graph) =
  withImpl impl \proxy -> do
    let name = implName proxy <> " " <> graph.name <> " update+read"
    fnEff name do
      root <- newP proxy 0
      let cell = graph.construct root.cell
      foreach_ bigArray $ mkEffFn1 \x -> do
        root.update x
        void (read cell)

benchRead :: Test
benchRead impl (Graph graph) =
  withImpl impl \proxy -> do
    let name = implName proxy <> " " <> graph.name <> " read"
    fnEff name do
      root <- newP proxy 0
      let cell = graph.construct root.cell
      foreach_ bigArray $ mkEffFn1 \x -> do
        void (read cell)

main :: Eff (console :: CONSOLE, st :: ST RealWorld) Unit
main = do

  for_ graphs \graph ->
    for_ tests \test ->
      runBench do
        for_ impls \impl ->
          test impl graph

newtype Graph = Graph
  { construct :: forall f. Applicative f => f Int -> f Int
  , name :: String
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
