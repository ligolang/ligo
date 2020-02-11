import { ActionType as ExamplesActionType, ChangeSelectedAction as ChangeSelectedExampleAction } from './examples';

export enum MichelsonFormat {
  Text = 'text',
  Json = 'json'
}

export enum ActionType {
  ChangeEntrypoint = 'compile-change-entrypoint'
}

export interface CompileState {
  entrypoint: string;
}

export class ChangeEntrypointAction {
  public readonly type = ActionType.ChangeEntrypoint;
  constructor(public payload: CompileState['entrypoint']) {}
}

type Action = ChangeEntrypointAction | ChangeSelectedExampleAction;

const DEFAULT_STATE: CompileState = {
  entrypoint: ''
};

export default (state = DEFAULT_STATE, action: Action): CompileState => {
  switch (action.type) {
    case ExamplesActionType.ChangeSelected:
      return {
        ...state,
        ...(!action.payload ? DEFAULT_STATE : action.payload.compile)
      };
    case ActionType.ChangeEntrypoint:
      return {
        ...state,
        entrypoint: action.payload
      };
  }
  return state;
};
