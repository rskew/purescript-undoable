module Data.Stackable where

import Prelude
import Data.Identity (Identity)
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple (..))


class StackableM m item stack where
  pushM :: item -> stack item -> m (stack item)
  popM :: stack item -> m (Maybe (Tuple (stack item) item))
  semptyM :: m (stack item)

class Stackable item stack where
  push :: item -> stack item -> stack item
  pop :: stack item -> Maybe (Tuple (stack item) item)
  sempty :: stack item

instance stackMStackable :: Stackable item stack => StackableM Identity item stack where
  pushM item = pure <<< push item
  popM = pure <<< pop
  semptyM = pure sempty

instance stackMList :: Stackable item List where
  push item list = item : list
  pop list =
    case List.uncons list of
      Nothing -> Nothing
      Just uncons ->
        Just $ Tuple uncons.tail uncons.head
  sempty = Nil
