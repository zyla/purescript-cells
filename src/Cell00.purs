-- | Cells that do no caching. Heavily optimized with EffFn.
module Cell00
  ( Cell
  ) where

import Prelude

import Control.Monad.Eff.Uncurried (mkEffFn1, mkEffFn2, runEffFn1, runEffFn2)
import Effect (Effect, Ref, newRefSlow, readRef, writeRef)
import IsCell as C
import Unsafe.Coerce (unsafeCoerce)

newtype Cell a = Cell (Effect a)

derive newtype instance functorCell :: Functor Cell
derive newtype instance applyCell :: Apply Cell
derive newtype instance applicativeCell :: Applicative Cell

type Root = Ref

toRoot :: forall a. Root a -> C.Root Cell a
toRoot = unsafeCoerce

fromRoot :: forall a. C.Root Cell a -> Root a
fromRoot = unsafeCoerce

instance isCellCell :: C.IsCell Cell where

  new x = do
    ref <- newRefSlow x
    pure (toRoot ref)

  readRoot root = Cell (runEffFn1 readRef (fromRoot root))

  update = mkEffFn2 \root value -> do
    runEffFn2 writeRef (fromRoot root) value

  read = mkEffFn1 \(Cell source) -> source

  implName _ = "Cell00"
