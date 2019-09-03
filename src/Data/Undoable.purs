module Data.Undoable where

import Prelude

import Data.Array as Array
import Data.Collapsable (class Collapsable, collapse)
import Data.Group (class Group, ginverse)
import Data.Identity (Identity(..))
import Data.Lens (Lens, Lens', lens)
import Data.Lens.Record (prop)
import Data.List (List(..))
import Data.List as List
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid.Action (class Action, class ActionM, actM)
import Data.Symbol (SProxy(..))
import Data.Generic.Rep (class Generic)
import Foreign.Class (class Encode, class Decode)
import Foreign.Generic (genericEncode, genericDecode, defaultOptions)

-- | Store the actions performed on your state so that they can be undone.
-- |
-- | Requires actions to be undoable, i.e. have a group structure.

type UndoableInner val action
  = { current :: val
    , history :: List action
    , undone :: List action
    }

newtype Undoable val action = Undoable (UndoableInner val action)

_Undoable :: forall v v' a a'. Lens (Undoable v a) (Undoable v' a') (UndoableInner v a) (UndoableInner v' a')
_Undoable = lens (\(Undoable a) -> a) (\_ -> Undoable)

_current :: forall v v' a. Lens (Undoable v a) (Undoable v' a) v v'
_current = _Undoable <<< prop (SProxy :: SProxy "current")

_history :: forall v a. Lens' (Undoable v a) (List a)
_history = _Undoable <<< prop (SProxy :: SProxy "history")

_undone :: forall v a. Lens' (Undoable v a) (List a)
_undone = _Undoable <<< prop (SProxy :: SProxy "undone")

initUndoable :: forall val action. val -> Undoable val action
initUndoable val = Undoable { current : val, history : Nil, undone : Nil }

mapActions :: forall val action action'. (action -> action') -> Undoable val action -> Undoable val action'
mapActions f (Undoable undoable) =
  Undoable
  { current : undoable.current
  , history : map f undoable.history
  , undone : map f undoable.undone
  }

-- | Perform an action and record it in the history.
-- |
-- | Actions can be defined to collapse together to be represented in
-- | history as a single action. For example, drag-actions can be
-- | collapsed together so that the entire drag motion can be undone with
-- | a single undo.
-- |
-- | Don't reset the record of undone actions when doing a new action.
-- | This means you can undo some actions, do some more actions,
-- | Then redo the old undone actions.
-- |
-- | This may not be good or intuitive and may change in future versions :upside_down_face:
dooM :: forall m val action. ActionM m action val => Group action => Collapsable action =>
        action -> Undoable val action -> m (Undoable val action)
dooM action (Undoable undoState) =
  let
    -- Collapse action with the last recorded action if possible
    maybeCollapsedHistory = do
      history' <- List.uncons undoState.history
      collapsedAction <- collapse action history'.head
      pure $ Cons collapsedAction history'.tail
    history = fromMaybe
              (Cons action undoState.history)
              maybeCollapsedHistory
  in do
    updatedState <- actM action undoState.current
    pure $ Undoable
           { current : updatedState
           , history : history
           , undone : undoState.undone
           }

doo :: forall action val. Action action val => Group action => Collapsable action =>
       action -> Undoable val action -> Undoable val action
doo action undoState =
  let
    Identity updatedUndoable = dooM action undoState
  in
    updatedUndoable

undoM :: forall m val action. ActionM m action val => Group action =>
         Undoable val action -> m (Undoable val action)
undoM (Undoable undoState) =
  case List.uncons undoState.history of
    Nothing -> pure $ Undoable undoState
    Just history ->
      let
        lastAction = history.head
        restHistory = history.tail
      in do
        updatedState <- actM (ginverse lastAction) undoState.current
        pure $ Undoable
               { current : updatedState
               , history : restHistory
               , undone : Cons lastAction undoState.undone
               }

undo :: forall action val. Action action val => Group action =>
         Undoable val action -> Undoable val action
undo undoState =
  let
    Identity updatedUndoable = undoM undoState
  in
    updatedUndoable

redoM :: forall m val action. ActionM m action val => Group action =>
         Undoable val action -> m (Undoable val action)
redoM (Undoable undoState) =
  case List.uncons undoState.undone of
    Nothing -> pure $ Undoable undoState
    Just undone ->
      let
        lastUndoneAction = undone.head
        restUndone = undone.tail
      in do
        updatedState <- actM lastUndoneAction undoState.current
        pure $ Undoable
               { current : updatedState
               , history : Cons lastUndoneAction undoState.history
               , undone : restUndone
               }

redo :: forall action val. Action action val => Group action =>
        Undoable val action -> Undoable val action
redo undoState =
  let
    Identity updatedUndoable = redoM undoState
  in
    updatedUndoable


------
-- Serialisation/deserialisation

newtype ForeignUndoable val action =
  ForeignUndoable
  { current :: val
  , history :: Array action
  , undone :: Array action
  }
derive instance genericForeignUndoable :: Generic (ForeignUndoable val action) _
instance encodeForeignUndoable :: (Encode val, Encode action) => Encode (ForeignUndoable val action) where
  encode = genericEncode defaultOptions
instance decodeForeignUndoable :: (Decode val, Decode action) => Decode (ForeignUndoable val action) where
  decode = genericDecode defaultOptions

toForeign :: forall val action. Undoable val action -> ForeignUndoable val action
toForeign (Undoable undoable) =
  ForeignUndoable
  { current : undoable.current
  , history : Array.fromFoldable undoable.history
  , undone : Array.fromFoldable undoable.undone
  }

fromForeign :: forall val action. ForeignUndoable val action -> Undoable val action
fromForeign (ForeignUndoable foreignUndoable) =
  Undoable
  { current : foreignUndoable.current
  , history : List.fromFoldable foreignUndoable.history
  , undone : List.fromFoldable foreignUndoable.undone
  }

derive instance genericUndoable :: Generic (Undoable val action) _

instance encodeUndoable :: (Encode val, Encode action) => Encode (Undoable val action) where
  encode = toForeign >>> genericEncode defaultOptions

instance decodeUndoable :: (Decode val, Decode action) => Decode (Undoable val action) where
  decode = map fromForeign <<< genericDecode defaultOptions
