module Cell5
  ( Cell
  ) where

import Prelude

import Cell4 (unsafeIsNull, unsafeNull)
import Control.Monad.Eff.Uncurried (EffFn1, EffFn2, mkEffFn1, mkEffFn2, runEffFn1, runEffFn2)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Data.Exists (Exists, mkExists, runExists)
import Data.Tuple (Tuple(..))
import Effect (Effect, Ref, E, newRef, readRef, writeRef)
import IsCell as C
import Unsafe.Coerce (unsafeCoerce)

-----------------------------------------
-- time stuff
-----------------------------------------

type Time = Int
data Timed a = Timed Time a

derive instance functorTimed :: Functor Timed

unTimed :: forall a. Timed a -> a
unTimed (Timed _ a) = a

nextTimeRef :: Ref Time
nextTimeRef = unsafePerformEff (runEffFn1 newRef 1)

nextTime :: Effect Time
nextTime = do
  tm <- runEffFn1 readRef nextTimeRef
  let tm' = tm + 1
  runEffFn2 writeRef nextTimeRef tm'
  pure tm'

-----------------------------------------

newtype Cell a = Derived (Exists (DerivedF a))

data DerivedF a b = DerivedF (Ref (Timed a)) (EffFn1 E Time (Timed b)) (b -> a)

instance functorCell :: Functor Cell where
  map f cell = mkDerived (mkEffFn1 \now -> runEffFn2 readTimed now cell) f

instance applyCell :: Apply Cell where
  apply fCell xCell = mkDerived source fn
    where
      source = mkEffFn1 \now -> do
        Timed fTime f <- runEffFn2 readTimed now fCell
        Timed xTime x <- runEffFn2 readTimed now xCell
        pure (Timed (max fTime xTime) (Tuple f x))

      fn (Tuple f x) = f x

instance applicativeCell :: Applicative Cell where
  pure x = mkDerived (mkEffFn1 \_now -> pure (Timed 0 x)) id

-- TODO: figure out the Monad instance

mkDerived :: forall a b. (EffFn1 E Time (Timed b)) -> (b -> a) -> Cell a
mkDerived source fn = unsafePerformEff do
  cache <- runEffFn1 newRef unsafeNull
  pure (Derived (mkExists (DerivedF cache source fn)))

readTimed :: forall a. EffFn2 E Time (Cell a) (Timed a)
readTimed = mkEffFn2 \now cell ->
  case cell of
    Derived ex -> do
      let DerivedF cache source fn = unsafeCoerce ex
      cached <- runEffFn1 readRef cache
      if unsafeIsNull cached
        then do
          -- Never updated. Recompute.
          Timed srcTime value <- runEffFn1 source now
          let updated = Timed srcTime (fn value)
          runEffFn2 writeRef cache updated
          pure updated

        else do
          let Timed cacheTime _ = cached
          if cacheTime >= now
            then do
              -- Already computed in this frame. No need to do anything.
              pure cached
            else do
              Timed srcTime value <- runEffFn1 source now
              if srcTime <= cacheTime
                then
                  -- Source is not newer than our cache. No need to do anything.
                  pure cached
                else do
                  -- Source was updated later than our cache. Recompute.
                  let updated = Timed now (fn value)
                  runEffFn2 writeRef cache updated
                  pure updated

withExists :: forall f r. Exists f -> (forall a. f a -> r) -> r
withExists f ex = runExists ex f

type Root a = Ref (Timed a)

toRoot :: forall a. Root a -> C.Root Cell a
toRoot = unsafeCoerce

fromRoot :: forall a. C.Root Cell a -> Root a
fromRoot = unsafeCoerce

instance isCellCell :: C.IsCell Cell where
  new x = do
    ref <- runEffFn1 newRef (Timed 0 x) -- TODO: is "0" correct?
    pure (toRoot ref)

  readRoot root = mkDerived (mkEffFn1 \_time -> runEffFn1 readRef (fromRoot root)) id

  update = mkEffFn2 \root value -> do
    time <- nextTime
    runEffFn2 writeRef (fromRoot root) (Timed time value)

  read = mkEffFn1 \cell -> do
    currentTime <- runEffFn1 readRef nextTimeRef
    Timed _ x <- runEffFn2 readTimed currentTime cell
    pure x

  implName _ = "Cell5"
