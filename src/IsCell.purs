module IsCell
  ( class IsCell
  , new
  , read
  , implName
  , newP

  , FProxy(..)
  ) where

import Prelude

import Effect (Effect)

data FProxy (f :: Type -> Type) = FProxy

class Applicative f <= IsCell f where
  new :: forall a. a -> Effect { cell :: f a, update :: a -> Effect Unit }
  read :: forall a. f a -> Effect a
  implName :: FProxy f -> String

newP :: forall f a. IsCell f => FProxy f -> a -> Effect { cell :: f a, update :: a -> Effect Unit }
newP _ = new
