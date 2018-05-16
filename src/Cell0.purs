-- | Cells that do no caching.
module Cell0
  ( Cell
  , new
  , read
  ) where

import Prelude

import Effect (Effect, newRef, readRef, writeRef)

newtype Cell a = Cell (Effect a)

derive newtype instance functorCell :: Functor Cell
derive newtype instance applyCell :: Apply Cell
derive newtype instance applicativeCell :: Applicative Cell

new :: forall a. a -> Effect { cell :: Cell a, update :: a -> Effect Unit }
new x = do
  ref <- newRef x
  pure
    { cell: Cell (readRef ref)
    , update: \value -> writeRef ref value
    }

read :: forall a. Cell a -> Effect a
read (Cell source) = source
