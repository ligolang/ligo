import { CancellableAction } from './actions/cancellable';
import { Command } from './types';

export enum ActionType {
  ChangeSelected = 'command-change-selected',
  ChangeDispatchedAction = 'command-change-dispatched-action'
}

export interface CommandState {
  selected: Command;
  dispatchedAction: CancellableAction | null;
}

export class ChangeSelectedAction {
  public readonly type = ActionType.ChangeSelected;
  constructor(public payload: CommandState['selected']) {}
}

export class ChangeDispatchedAction {
  public readonly type = ActionType.ChangeDispatchedAction;
  constructor(public payload: CommandState['dispatchedAction']) {}
}

type Action = ChangeSelectedAction | ChangeDispatchedAction;

const DEFAULT_STATE: CommandState = {
  selected: Command.Compile,
  dispatchedAction: null
};

export default (state = DEFAULT_STATE, action: Action): CommandState => {
  switch (action.type) {
    case ActionType.ChangeSelected:
      return {
        ...state,
        selected: action.payload
      };
    case ActionType.ChangeDispatchedAction:
      return {
        ...state,
        dispatchedAction: action.payload
      };
  }
  return state;
};
