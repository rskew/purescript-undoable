module Data.Monoid.Action where

import Prelude
import Data.Identity (Identity)

-- | directly from https://hackage.haskell.org/package/monoid-extras-0.5/docs/src/Data.Monoid.Action.html
class Monoid m <= Action m s where
  -- | Convert a value of type @m@ to an action on @s@ values.
  act :: m -> s -> s

class (Monoid m, Monad e) <= ActionM e m s where
  -- | Convert a value of type @m@ to a monadic action on @s@ values.
  actM :: m -> s -> e s

instance identityActionM :: Action m s => ActionM Identity m s where
  actM m s = pure $ act m s
