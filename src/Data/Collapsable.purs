module Data.Collapsable where

import Data.Maybe (Maybe)

-- | Data that can have values collapsed toagether.
-- |
-- | For declaring particular subsets that can be collapsed,
-- | as opposed to Semigroup which declares that all data can
-- | be collapsed.
-- |
-- | Used in Undoable for collapsing actions such as a stream of
-- | mouse drag actions into a single action.
class Collapsable a where
  collapse :: a -> a -> Maybe a
