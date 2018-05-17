module Effect
  ( Effect
  , E
  , RealWorld
  , Ref

  , newRefSlow
  , readRefSlow
  , writeRefSlow

  , newRef
  , readRef
  , writeRef
  ) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Uncurried (EffFn1, EffFn2, mkEffFn1)
import Control.Monad.ST (ST, STRef, newSTRef, readSTRef, writeSTRef)
import Unsafe.Coerce (unsafeCoerce)

foreign import data RealWorld :: Type

type E = (st :: ST RealWorld)

type Effect = Eff E

type Ref = STRef RealWorld

newRefSlow :: forall a. a -> Effect (Ref a)
newRefSlow = newSTRef

readRefSlow :: forall a. Ref a -> Effect a
readRefSlow = readSTRef

writeRefSlow :: forall a. Ref a -> a -> Effect Unit
writeRefSlow ref value = void (writeSTRef ref value)

newRef :: forall a. EffFn1 E a (Ref a)
newRef = mkEffFn1 \x -> pure (unsafeCoerce { value: x })

readRef :: forall a. EffFn1 E (Ref a) a
readRef = mkEffFn1 \ref -> pure (unsafeCoerce ref).value

foreign import writeRef :: forall a. EffFn2 E (Ref a) a Unit
