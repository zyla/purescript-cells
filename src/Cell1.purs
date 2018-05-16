module Cell1
  ( Cell
  ) where

import Prelude

import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Control.Monad.ST (modifySTRef)
import Data.Exists (Exists, mkExists, runExists)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect, Ref, newRef, readRef, writeRef)
import IsCell (class IsCell)

-----------------------------------------
-- time stuff
-----------------------------------------

type Time = Int
data Timed a = Timed Time a

derive instance functorTimed :: Functor Timed

unTimed :: forall a. Timed a -> a
unTimed (Timed _ a) = a

nextTimeRef :: Ref Time
nextTimeRef = unsafePerformEff (newRef 1)

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
  cache <- newRef Nothing
  pure (Derived (mkExists (DerivedF cache source fn)))

readTimed :: forall a. Time -> Cell a -> Effect (Timed a)
readTimed _   (Root source) = source
readTimed now (Derived ex) =
  withExists ex \(DerivedF cache source fn) -> do
    m_cached <- readRef cache
    case m_cached of
      Nothing -> do
        -- Never updated. Recompute.
        Timed srcTime value <- source now
        let updated = Timed srcTime (fn value)
        writeRef cache (Just updated)
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
                writeRef cache (Just updated)
                pure updated

withExists :: forall f r. Exists f -> (forall a. f a -> r) -> r
withExists f ex = runExists ex f

instance isCellCell :: IsCell Cell where
  new x = do
    ref <- newRef (Timed 0 x) -- TODO: is "0" correct?
    pure
      { cell: Root (readRef ref)
      , update: \value -> do
         time <- nextTime
         writeRef ref (Timed time value)
      }

  read cell = do
    currentTime <- readRef nextTimeRef
    map unTimed (readTimed currentTime cell)

  implName _ = "Cell1"
