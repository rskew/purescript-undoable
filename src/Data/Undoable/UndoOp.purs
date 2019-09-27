module Data.Undoable.UndoOp where

import Prelude

import Data.Collapsable (class Collapsable)
import Data.Group (class Group)
import Data.Monoid.Action (class ActionM, class Action)
import Data.Stackable (class StackableM, class Stackable)
import Data.Undoable (Undoable, dooM', dooM, doo', doo, redoM', redoM, redo', redo, undoM', undoM, undo', undo)
import Run (FProxy, Run)
import Run as Run
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))

-- | Store the actions performed on your state so that they can be undone.
-- |
-- | Requires actions to be undoable, i.e. have a group structure.

data UndoOpF action a
  = Do action a
  | Undo a
  | Redo a

derive instance functorUndoF :: Functor (UndoOpF action)

type UNDOOP action = FProxy (UndoOpF action)

_undoOp :: SProxy "undoOp"
_undoOp = SProxy

dooOp :: forall action r. action -> Run (undoOp :: UNDOOP action | r) Unit
dooOp action = Run.lift _undoOp $ Do action unit

undoOp :: forall action r. Run (undoOp :: UNDOOP action | r) Unit
undoOp = Run.lift _undoOp $ Undo unit

redoOp :: forall action r. Run (undoOp :: UNDOOP action | r) Unit
redoOp = Run.lift _undoOp $ Redo unit

handleUndoOpM' :: forall a val action stack m m'.
                  ActionM m action val
                  => Group action
                  => Monad m'
                  => StackableM m' action stack
                  => Collapsable action
                  => UndoOpF action a
                  -> Tuple (Undoable val action stack -> m' (m (Undoable val action stack))) a
handleUndoOpM' = case _ of
  Do action next -> Tuple (dooM' action) next
  Undo next      -> Tuple undoM'         next
  Redo next      -> Tuple redoM'         next

handleUndoOpM :: forall a val action stack m.
                 ActionM m action val
                 => Group action
                 => Stackable action stack
                 => Collapsable action
                 => UndoOpF action a
                 -> Tuple (Undoable val action stack -> m (Undoable val action stack)) a
handleUndoOpM = case _ of
  Do action next -> Tuple (dooM action) next
  Undo next      -> Tuple undoM         next
  Redo next      -> Tuple redoM         next

handleUndoOp' :: forall a val action stack m'.
                 Action action val
                 => Group action
                 => Monad m'
                 => StackableM m' action stack
                 => Collapsable action
                 => UndoOpF action a
                 -> Tuple (Undoable val action stack -> m' (Undoable val action stack)) a
handleUndoOp' = case _ of
  Do action next -> Tuple (doo' action) next
  Undo next      -> Tuple undo'         next
  Redo next      -> Tuple redo'         next

handleUndoOp :: forall a val action stack.
                Action action val
                => Group action
                => Stackable action stack
                => Collapsable action
                => UndoOpF action a
                -> Tuple (Undoable val action stack -> Undoable val action stack) a
handleUndoOp = case _ of
  Do action next -> Tuple (doo action) next
  Undo next      -> Tuple undo         next
  Redo next      -> Tuple redo         next
