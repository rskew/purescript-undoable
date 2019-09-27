module Test.Main where

import Prelude
import Effect (Effect)
import Data.Collapsable (class Collapsable)
import Data.Group (class Group)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Monoid.Action (class Action)
import Data.Tuple (Tuple(..))
import Data.Undoable (Undoable(..), initUndoable, doo, undo, redo)
import Test.Assert (assertEqual)

newtype Counter = Counter Int

instance showCounter :: Show Counter where
  show (Counter c) = "Counter " <> show c

derive instance eqCounter :: Eq Counter

newtype Increment = Increment Int

derive instance eqIncrement :: Eq Increment

instance showIncrement :: Show Increment where
  show (Increment i) = "Increment " <> show i

instance actionCounterIncrement :: Action Increment Counter where
  act (Increment i) (Counter c) = Counter (c + i)

instance semigroupIncrement :: Semigroup Increment where
  append (Increment i) (Increment j) = Increment (i + j)

instance monoidIncrement :: Monoid Increment where
  mempty = Increment 0

instance groupIncrement :: Group Increment where
  ginverse (Increment i) = Increment (-i)

instance collapsableIncrement :: Collapsable Increment where
  collapse (Increment i) (Increment j) = case Tuple (i < 0) (j < 0) of
    Tuple true true -> Just (Increment (i + j))
    _ -> Nothing

main :: Effect Unit
main = do
  -- Simple counting
  assertEqual { actual :
                (initUndoable (Counter 0) :: Undoable Counter Increment List)
                # doo (Increment 3)
                # doo (Increment 3)
                # doo (Increment 2)
              , expected :
                Undoable { current : (Counter 8)
                         , history : Increment 2 : Increment 3 : Increment 3 : Nil
                         , undone  : Nil
                         }
              }

  -- Undoing
  assertEqual { actual :
                (initUndoable (Counter 0) :: Undoable Counter Increment List)
                # doo (Increment 5)
                # doo (Increment 5)
                # undo
                # undo
              , expected :
                Undoable { current : Counter 0
                         , history : Nil
                         , undone  : Increment 5 : Increment 5 : Nil
                         }
              }

  -- Multiple undos
  assertEqual { actual :
                (initUndoable (Counter 0) :: Undoable Counter Increment List)
                # doo (Increment 20)       -- current: 20, history: [Increment 20],              undone: []
                # doo (Increment 2)      -- current: 22, history: [Increment 2, Increment 20], undone: []
                # undo                   -- current: 20, history: [Increment 20],              undone: [Increment 2]
                # undo                   -- current: 0,  history: [],                          undone: [Increment 20, Increment 2]
                # undo                   -- current: 0,  history: [],                          undone: [Increment 20, Increment 2]
                # doo (Increment 1)      -- current: 1,  history: [Increment 1],               undone: [Increment 20, Increment 2]
              , expected :
                Undoable { current : Counter 1
                         , history : Increment 1 : Nil
                         , undone  : Increment 20 : Increment 2 : Nil
                         }
              }

  -- With redo
  assertEqual { actual :
                (initUndoable (Counter 0)
                 :: Undoable Counter Increment List) -- current: 0,  history: [],             undone: []
                # doo (Increment 20)                 -- current: 20, history: [Increment 20], undone: []
                # undo                             -- current: 0 , history: [],             undone: [Increment 20]
                # redo                             -- current: 20, history: [Increment 20], undone: []
              , expected :
                Undoable { current : Counter 20
                         , history : Increment 20 : Nil
                         , undone  : Nil
                         }
              }

  -- Collapsing
  assertEqual { actual :
                (initUndoable (Counter 0)
                 :: Undoable Counter Increment List) -- current: 0,  history: [],             undone: []
                # doo (Increment (-20))              -- current: -20, history: [Increment (-20)], undone: []
                # doo (Increment (-4))             -- current: -24, history: [Increment (-24)], undone: []
                # doo (Increment (-1))             -- current: -25, history: [Increment (-25)], undone: []
                # undo                             -- current: 0,   history: [],                undone: [Increment (-24)]
              , expected :
                Undoable { current : Counter 0
                         , history : Nil
                         , undone  : Increment (-25) : Nil
                         }
              }
