module Effect
  ( Effect
  , E
  , RealWorld
  , Ref
  , newRef
  , readRef
  , writeRef
  ) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.ST (ST, STRef, newSTRef, readSTRef, writeSTRef)

foreign import data RealWorld :: Type

type E = (st :: ST RealWorld)

type Effect = Eff E

type Ref = STRef RealWorld

newRef :: forall a. a -> Effect (Ref a)
newRef = newSTRef

readRef :: forall a. Ref a -> Effect a
readRef = readSTRef

writeRef :: forall a. Ref a -> a -> Effect Unit
writeRef ref value = void (writeSTRef ref value)
