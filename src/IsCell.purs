module IsCell
  ( class IsCell
  , Root
  , new
  , readRoot
  , update
  , read
  , implName
  , newP

  , FProxy(..)
  ) where

import Prelude

import Control.Monad.Eff.Uncurried (EffFn1, EffFn2)
import Effect (Effect, E)

data FProxy (f :: Type -> Type) = FProxy

data Root (cell :: Type -> Type) (a :: Type)

class Applicative f <= IsCell f where
  new :: forall a. a -> Effect (Root f a)
  readRoot :: forall a. Root f a -> f a
  update :: forall a. EffFn2 E (Root f a) a Unit
  read :: forall a. EffFn1 E (f a) a
  implName :: FProxy f -> String

newP :: forall f a. IsCell f => FProxy f -> a -> Effect (Root f a)
newP _ = new
