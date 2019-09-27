-- | Store the history of actions performed on your state, allowing them to be undone.
-- |
-- | Requires actions to be undoable, i.e. have a group structure.
module Data.Undoable where

import Prelude

import Data.Array as Array
import Data.Collapsable (class Collapsable, collapse)
import Data.Generic.Rep (class Generic)
import Data.Group (class Group, ginverse)
import Data.Identity (Identity(..))
import Data.Lens (Lens, Lens', lens)
import Data.Lens.Record (prop)
import Data.List (List)
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Monoid.Action (class Action, class ActionM, actM)
import Data.Stackable (class StackableM, popM, pushM, semptyM, class Stackable, sempty)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Foreign.Class (class Encode, class Decode)
import Foreign.Generic (genericEncode, genericDecode, defaultOptions)

type UndoableInner val action stack
  = { current :: val
    , history :: stack action
    , undone :: stack action
    }

newtype Undoable val action stack = Undoable (UndoableInner val action stack)

instance eqUndoable :: (Eq val, Eq action, Eq (stack action)) => Eq (Undoable val action stack) where
  eq (Undoable a) (Undoable b) = a == b

instance showUndoable :: (Show val, Show action, Show (stack action)) => Show (Undoable val action stack) where
  show (Undoable undoable) =
   "Undoable { current: " <> show undoable.current <>
   ", history: " <> show undoable.history <>
   " undone: " <> show undoable.undone <>
   " }"

_Undoable :: forall v v' a a' s. Lens (Undoable v a s) (Undoable v' a' s) (UndoableInner v a s) (UndoableInner v' a' s)
_Undoable = lens (\(Undoable a) -> a) (\_ -> Undoable)

_current :: forall v v' a s. Lens (Undoable v a s) (Undoable v' a s) v v'
_current = _Undoable <<< prop (SProxy :: SProxy "current")

_history :: forall v a s. Lens' (Undoable v a s) (s a)
_history = _Undoable <<< prop (SProxy :: SProxy "history")

_undone :: forall v a s. Lens' (Undoable v a s) (s a)
_undone = _Undoable <<< prop (SProxy :: SProxy "undone")

initUndoable' :: forall val action stack m. Monad m => StackableM m action stack =>
                 val -> m (Undoable val action stack)
initUndoable' val = do -- m
  history <- semptyM
  undone <- semptyM
  pure $ Undoable { current : val, history : history, undone : undone }

-- | Specialisation to the case of pure Stackable
initUndoable :: forall val action stack.
                Stackable action stack
                => val
                -> Undoable val action stack
initUndoable val =
  Undoable { current : val, history : sempty, undone : sempty }

mapActions :: forall val action action' stack.
              Functor stack
              => (action -> action')
              -> Undoable val action stack
              -> Undoable val action' stack
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
dooM' :: forall m val action stack m'.
         ActionM m action val
         => Group action
         => Collapsable action
         => Monad m'
         => StackableM m' action stack
         => action
         -> Undoable val action stack
         -> m' (m (Undoable val action stack))
dooM' action (Undoable undoState) = do -- m'
  maybeUnstack <- popM undoState.history
  newHistory <- case maybeUnstack of
    Nothing -> pushM action undoState.history
    Just (Tuple restHistory lastAction) ->
      case collapse action lastAction of
        Nothing -> do
          restHistory
          # pushM lastAction
          >>= pushM action
        Just collapsedAction ->
          pushM collapsedAction restHistory
  pure do -- m
    updatedState <- actM action undoState.current
    pure $ Undoable
           { current : updatedState
           , history : newHistory
           , undone : undoState.undone
           }

-- | Specialisation to the case of pure StackableM
dooM :: forall m val action stack.
        ActionM m action val
        => Group action
        => Collapsable action
        => StackableM Identity action stack
        => action
        -> Undoable val action stack
        -> m (Undoable val action stack)
dooM action undoableM =
  let
    Identity undoable = dooM' action undoableM
  in
    undoable

-- | Specialisation to the case of pure Action
doo' :: forall action val m stack.
        Action action val
        => Group action
        => Collapsable action
        => Monad m
        => StackableM m action stack
        => action
        -> Undoable val action stack
        -> m (Undoable val action stack)
doo' action undoState = do
  -- Action has an ActionM instance using Identity
  Identity updatedUndoable <- dooM' action undoState
  pure updatedUndoable

-- | Specialisation to the case of pure Action and pure Stackable
doo :: forall action val stack.
       Action action val
       => Group action
       => Collapsable action
       => Stackable action stack
       => action
       -> Undoable val action stack
       -> Undoable val action stack
doo action undoState =
  let
    Identity updatedUndoable = doo' action undoState
  in
    updatedUndoable

undoM' :: forall m val action m' stack.
          ActionM m action val
          => Group action
          => Monad m'
          => StackableM m' action stack
          => Undoable val action stack
          -> m' (m (Undoable val action stack))
undoM' (Undoable undoState) = do -- m'
  maybeUnstack <- popM undoState.history
  case maybeUnstack of
    Nothing -> pure $ pure $ Undoable undoState
    Just (Tuple restHistory lastAction) -> do -- m'
      updatedUndone <- pushM lastAction undoState.undone
      pure do -- m
        updatedState <- actM (ginverse lastAction) undoState.current
        pure $ Undoable
               { current : updatedState
               , history : restHistory
               , undone : updatedUndone
               }

-- | Specialisation to the case of pure Stackable
undoM :: forall m val action stack.
        ActionM m action val
        => Group action
        => Collapsable action
        => Stackable action stack
        => Undoable val action stack
        -> m (Undoable val action stack)
undoM undoableM =
  let
    Identity undoable = undoM' undoableM
  in
   undoable

-- | Specialisation to the case of pure Action
undo' :: forall action val m' stack.
         Action action val
         => Group action
         => Monad m'
         => StackableM m' action stack
         => Undoable val action stack
         -> m' (Undoable val action stack)
undo' undoState = do -- m'
  Identity updatedUndoable <- undoM' undoState
  pure updatedUndoable

-- | Specialisation to the case of pure Action and pure Stackable
undo :: forall action val stack.
     Action action val
     => Group action
     => Stackable action stack
     => Undoable val action stack
     -> Undoable val action stack
undo undoState =
  let
    Identity updatedUndoable = undo' undoState
  in
    updatedUndoable

redoM' :: forall m val action m' stack.
          ActionM m action val
          => Group action
          => Monad m'
          => StackableM m' action stack
          => Undoable val action stack
          -> m' (m (Undoable val action stack))
redoM' (Undoable undoState) = do -- m'
  maybeUnstack <- popM undoState.undone
  case maybeUnstack of
    Nothing -> pure $ pure $ Undoable undoState
    Just (Tuple restUndone lastUndoneAction) -> do -- m'
      updatedHistory <- pushM lastUndoneAction undoState.history
      pure do -- m
        updatedState <- actM lastUndoneAction undoState.current
        pure $ Undoable
               { current : updatedState
               , history : updatedHistory
               , undone : restUndone
               }

-- | Specialisation to the case of pure Stackable
redoM :: forall m val action stack.
         ActionM m action val
         => Group action
         => Collapsable action
         => Stackable action stack
         => Undoable val action stack
         -> m (Undoable val action stack)
redoM undoableM =
  let
    Identity undoable = redoM' undoableM
  in
   undoable

-- | Specialisation to the case of pure Action
redo' :: forall action val m' stack.
         Action action val
         => Group action
         => Monad m'
         => StackableM m' action stack
         => Undoable val action stack
         -> m' (Undoable val action stack)
redo' undoState = do -- m'
  Identity updatedUndoable <- redoM' undoState
  pure updatedUndoable


-- | Specialisation to the case of pure Action and pure Stackable
redo :: forall action val stack.
        Action action val
        => Group action
        => Stackable action stack
        => Undoable val action stack
     -> Undoable val action stack
redo undoState =
  let
    Identity updatedUndoable = redo' undoState
  in
   updatedUndoable


------
-- Serialisation/deserialisation
--
-- Supporting List stacks

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

toForeign :: forall val action. Undoable val action List -> ForeignUndoable val action
toForeign (Undoable undoable) =
  ForeignUndoable
  { current : undoable.current
  , history : Array.fromFoldable undoable.history
  , undone : Array.fromFoldable undoable.undone
  }

fromForeign :: forall val action. ForeignUndoable val action -> Undoable val action List
fromForeign (ForeignUndoable foreignUndoable) =
  Undoable
  { current : foreignUndoable.current
  , history : List.fromFoldable foreignUndoable.history
  , undone : List.fromFoldable foreignUndoable.undone
  }

derive instance genericUndoable :: Generic (Undoable val action List) _

instance encodeUndoable :: (Encode val, Encode action) => Encode (Undoable val action List) where
  encode = toForeign >>> genericEncode defaultOptions

instance decodeUndoable :: (Decode val, Decode action) => Decode (Undoable val action List) where
  decode = map fromForeign <<< genericDecode defaultOptions
