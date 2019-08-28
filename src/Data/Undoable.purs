module Data.Undoable where

import Prelude

import Data.Collapsable (class Collapsable, collapse)
import Data.List (List(..))
import Data.List as List
import Data.Group (class Group, ginverse)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid.Action (class Action, act)
import Data.Lens (Lens', lens)
import Data.Lens.Record (prop)
import Data.Symbol (SProxy(..))

-- TODO: does Undoable really need to hold the val state?
type UndoableInner val action
  = { current :: val
    , history :: List action
    , undone :: List action
    }

newtype Undoable val action
  = Undoable (UndoableInner val action)

_Undoable :: forall v a. Lens' (Undoable v a) (UndoableInner v a)
_Undoable = lens (\(Undoable a) -> a) (\_ -> Undoable)

_current :: forall v a. Lens' (Undoable v a) v
_current = _Undoable <<< prop (SProxy :: SProxy "current")

_history :: forall v a. Lens' (Undoable v a) (List a)
_history = _Undoable <<< prop (SProxy :: SProxy "history")

_undone :: forall v a. Lens' (Undoable v a) (List a)
_undone = _Undoable <<< prop (SProxy :: SProxy "undone")

initUndoable :: forall val action. val -> Undoable val action
initUndoable val = Undoable { current : val, history : Nil, undone : Nil }

-- | Don't reset the record of undone actions when doing a new action.
-- | This means you can undo some actions, do some more actions,
-- | Then redo the old undone actions.
-- |
-- | This may not be good or intuitive and may change in future versions :upside_down_face:
doo :: forall val action. Action action val => Group action => Collapsable action =>
       action -> Undoable val action -> Undoable val action
doo action (Undoable undoState) =
  let
    -- Collapse action with the last recorded action if possible
    maybeCollapsedHistory = do
      history' <- List.uncons undoState.history
      collapsedAction <- collapse action history'.head
      pure $ Cons collapsedAction history'.tail
    history = fromMaybe
              (Cons action undoState.history)
              maybeCollapsedHistory
  in
    Undoable
    { current : act action undoState.current
    , history : history
    , undone : undoState.undone
    }

undo :: forall val action. Action action val => Group action =>
        Undoable val action -> Undoable val action
undo (Undoable undoState) =
  case List.uncons undoState.history of
    Nothing -> Undoable undoState
    Just history ->
      let
        lastAction = history.head
        restHistory = history.tail
      in
        Undoable
        { current : act (ginverse lastAction) undoState.current
        , history : restHistory
        , undone : Cons lastAction undoState.undone
        }

redo :: forall val action. Action action val => Group action =>
        Undoable val action -> Undoable val action
redo (Undoable undoState) =
  case List.uncons undoState.undone of
    Nothing -> Undoable undoState
    Just undone ->
      let
        lastUndoneAction = undone.head
        restUndone = undone.tail
      in
        Undoable
        { current : act lastUndoneAction undoState.current
        , history : Cons lastUndoneAction undoState.history
        , undone : restUndone
        }
