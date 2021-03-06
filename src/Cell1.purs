module Cell1
  ( Cell
  ) where

import Prelude

import Control.Monad.Eff.Uncurried (mkEffFn1, mkEffFn2)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Control.Monad.ST (modifySTRef)
import Data.Exists (Exists, mkExists, runExists)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect, Ref, newRefSlow, readRefSlow, writeRefSlow)
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
nextTimeRef = unsafePerformEff (newRefSlow 1)

nextTime :: Effect Time
nextTime = modifySTRef nextTimeRef (add 1)

-----------------------------------------

data Cell a =
    Root (Effect (Timed a))
  | Derived (Exists (DerivedF a))

data DerivedF a b = DerivedF (Ref (Maybe (Timed a))) (Time -> Effect (Timed b)) (b -> a)

instance functorCell :: Functor Cell where
  map f cell = mkDerived (\now -> readTimed now cell) f

instance applyCell :: Apply Cell where
  apply fCell xCell = mkDerived source fn
    where
      source now = do
        Timed fTime f <- readTimed now fCell
        Timed xTime x <- readTimed now xCell
        pure (Timed (max fTime xTime) (Tuple f x))

      fn (Tuple f x) = f x

instance applicativeCell :: Applicative Cell where
  pure x = Root (pure (Timed 0 x))

-- TODO: figure out the Monad instance

mkDerived :: forall a b. (Time -> Effect (Timed b)) -> (b -> a) -> Cell a
mkDerived source fn = unsafePerformEff do
  cache <- newRefSlow Nothing
  pure (Derived (mkExists (DerivedF cache source fn)))

readTimed :: forall a. Time -> Cell a -> Effect (Timed a)
readTimed _   (Root source) = source
readTimed now (Derived ex) =
  withExists ex \(DerivedF cache source fn) -> do
    m_cached <- readRefSlow cache
    case m_cached of
      Nothing -> do
        -- Never updated. Recompute.
        Timed srcTime value <- source now
        let updated = Timed srcTime (fn value)
        writeRefSlow cache (Just updated)
        pure updated

      Just cached@(Timed cacheTime _) ->
        if cacheTime >= now
          then do
            -- Already computed in this frame. No need to do anything.
            pure cached
          else do
            Timed srcTime value <- source now
            if srcTime <= cacheTime
              then
                -- Source is not newer than our cache. No need to do anything.
                pure cached
              else do
                -- Source was updated later than our cache. Recompute.
                let updated = Timed srcTime (fn value)
                writeRefSlow cache (Just updated)
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
    ref <- newRefSlow (Timed 0 x) -- TODO: is "0" correct?
    pure (toRoot ref)

  readRoot root = Root (readRefSlow (fromRoot root))

  update = mkEffFn2 \root value -> do
    time <- nextTime
    writeRefSlow (fromRoot root) (Timed time value)

  read = mkEffFn1 \cell -> do
    currentTime <- readRefSlow nextTimeRef
    map unTimed (readTimed currentTime cell)

  implName _ = "Cell1"
