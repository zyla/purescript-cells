module IsCell
  ( class IsCell
  , new
  , read
  , implName
  , newP

  , FProxy(..)
  ) where

import Prelude

import Control.Monad.Eff.Uncurried (EffFn1)
import Effect (Effect, E)

data FProxy (f :: Type -> Type) = FProxy

class Applicative f <= IsCell f where
  new :: forall a. a -> Effect { cell :: f a, update :: EffFn1 E a Unit }
  read :: forall a. EffFn1 E (f a) a
  implName :: FProxy f -> String

newP :: forall f a. IsCell f => FProxy f -> a -> Effect { cell :: f a, update :: EffFn1 E a Unit }
newP _ = new
