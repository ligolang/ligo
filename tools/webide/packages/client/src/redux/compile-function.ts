export enum ActionType {
  SetDefaultList = 'set-function',
  ChangeSelected = 'function-change-selected',
  ChangeProtocol = 'function-change-protocol',
}

export interface CompileFunctionState {
  functionName: string;
  protocol: string;
}

const DEFAULT_STATE = {
  functionName: '',
  protocol: 'kathmandu'
};

export class ChangeSelectedAction {
  public readonly type = ActionType.ChangeSelected;
  constructor(public payload: CompileFunctionState['functionName']) {}
}

export class ChangeProtocolAction {
  public readonly type = ActionType.ChangeProtocol;
  constructor(public payload: CompileFunctionState['protocol']) {}
}

const compileFunction = (
  state = DEFAULT_STATE,
  action: any
): CompileFunctionState => {
  switch (action.type) {
    case ActionType.ChangeSelected:
      return {
        ...state,
        functionName: action.payload ? action.payload : '',
      };
    case ActionType.ChangeProtocol:
      return {
        ...state,
        protocol: action.payload
      };
    case ActionType.SetDefaultList:
      return { ...state, functionName: action.value };
    default:
      return {
        ...state,
      };
  }
};

export default compileFunction;
