import { ActionType as ExamplesActionType, ChangeSelectedAction as ChangeSelectedExampleAction } from './examples';

export enum ActionType {
  ChangeEntrypoint = 'evaluate-expr-change-entrypoint',
  ChangeProtocol = 'evaluate-expr-change-protocol'
}

export interface EvaluateValueState {
  entrypoint: string;
  protocol: string;
}

export class ChangeEntrypointAction {
  public readonly type = ActionType.ChangeEntrypoint;
  constructor(public payload: EvaluateValueState['entrypoint']) {}
}

export class ChangeProtocolAction {
  public readonly type = ActionType.ChangeProtocol;
  constructor(public payload: EvaluateValueState['protocol']) {}
}


type Action = ChangeEntrypointAction | ChangeSelectedExampleAction | ChangeProtocolAction;

const DEFAULT_STATE: EvaluateValueState = {
  entrypoint: '',
  protocol: 'kathmandu'
};

const evaluateValue = (state = DEFAULT_STATE, action: Action): EvaluateValueState => {
  switch (action.type) {
    case ExamplesActionType.ChangeSelected:
      return {
        ...state,
        ...(!action.payload ? DEFAULT_STATE : action.payload.evaluateValue)
      };
    case ActionType.ChangeProtocol:
      return {
        ...state,
        protocol: action.payload
      };
    case ActionType.ChangeEntrypoint:
      return {
        ...state,
        entrypoint: action.payload
      };
    default:
      return state;
  }
};

export default evaluateValue;
