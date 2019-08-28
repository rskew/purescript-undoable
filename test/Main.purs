module Test.Main where

import Prelude
import Effect (Effect)
import Data.Collapsable (class Collapsable)
import Data.Group (class Group)
import Data.Lens (view)
import Data.Maybe (Maybe(..))
import Data.Monoid.Action (class Action)
import Data.Tuple (Tuple(..))
import Data.Undoable (_current, initUndoable, doo, undo, redo)
import Test.Assert (assertEqual)

newtype Counter = Counter Int

instance showCounter :: Show Counter where
  show (Counter c) = "Counter " <> show c

derive instance eqCounter :: Eq Counter

newtype Increment = Increment Int

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
                initUndoable (Counter 0)
                # doo (Increment 3)
                # doo (Increment 3)
                # doo (Increment 2)
                # view _current
              , expected : Counter 8
              }

  -- Undoing
  assertEqual { actual :
                initUndoable (Counter 0)
                # doo (Increment 5)
                # doo (Increment 5)
                # undo
                # undo
                # view _current
              , expected : Counter 0
              }

  -- Multiple undos
  assertEqual { actual :
                initUndoable (Counter 0)
                # doo (Increment 20)     -- current: 20, history: [Increment 20],              undone: []
                # doo (Increment 2)      -- current: 22, history: [Increment 2, Increment 20], undone: []
                # undo                   -- current: 20, history: [Increment 20],              undone: [Increment 2]
                # undo                   -- current: 0,  history: [],                          undone: [Increment 20, Increment 2]
                # undo                   -- current: 0,  history: [],                          undone: [Increment 20, Increment 2]
                # doo (Increment 1)      -- current: 1,  history: [Increment 1],               undone: [Increment 20, Increment 2]
                # view _current
              , expected : Counter 1
              }

  -- With redo
  assertEqual { actual :
                initUndoable (Counter 0) -- current: 0,  history: [],             undone: []
                # doo (Increment 20)     -- current: 20, history: [Increment 20], undone: []
                # undo                   -- current: 0 , history: [],             undone: [Increment 20]
                # redo                   -- current: 20, history: [Increment 20], undone: []
                # view _current
              , expected : Counter 20
              }

  -- Collapsing
  assertEqual { actual :
                initUndoable (Counter 0) -- current: 0,   history: [],                undone: []
                # doo (Increment (-20))  -- current: -20, history: [Increment (-20)], undone: []
                # doo (Increment (-4))   -- current: -24, history: [Increment (-24)], undone: []
                # doo (Increment (-1))   -- current: -25, history: [Increment (-25)], undone: []
                # undo                   -- current: 0,   history: [],                undone: [Increment (-24)]
                # view _current
              , expected : Counter 0
              }
