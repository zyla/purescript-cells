-- | Cells that do no caching.
module Cell0
  ( Cell
  ) where

import Prelude

import Control.Monad.Eff.Uncurried (mkEffFn1)
import Effect (Effect, newRef, readRef, writeRef)
import IsCell (class IsCell)

newtype Cell a = Cell (Effect a)

derive newtype instance functorCell :: Functor Cell
derive newtype instance applyCell :: Apply Cell
derive newtype instance applicativeCell :: Applicative Cell

instance isCellCell :: IsCell Cell where

  new x = do
    ref <- newRef x
    pure
      { cell: Cell (readRef ref)
      , update: mkEffFn1 (writeRef ref)
      }

  read = mkEffFn1 \(Cell source) -> source

  implName _ = "Cell0"
