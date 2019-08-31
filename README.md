Store the actions performed on your state so that they can be undone.

Rather than storing previous states to be reverted to, Undoable records tha actions performed, represented as data. An instance of Action is needed to interpret the action data into a change in state.

Requires actions to be undoable, i.e. have a group structure.

Actions can be defined to collapse together to be represented in history as a single action. For example, drag actions can be collapsed together so that the entire drag motion can be undone with a single undo.
