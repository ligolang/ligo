export enum ActionType {
  ChangeOutput = 'result-change-output',
  ChangeContract = 'result-change-contract'
}

export interface ResultState {
  output: string;
  contract: string;
}

export class ChangeOutputAction {
  public readonly type = ActionType.ChangeOutput;
  constructor(public payload: ResultState['output']) {}
}

export class ChangeContractAction {
  public readonly type = ActionType.ChangeContract;
  constructor(public payload: ResultState['contract']) {}
}

type Action = ChangeOutputAction | ChangeContractAction;

const DEFAULT_STATE: ResultState = {
  output: '',
  contract: ''
};

export default (state = DEFAULT_STATE, action: Action): ResultState => {
  switch (action.type) {
    case ActionType.ChangeOutput:
      return {
        ...state,
        output: action.payload
      };
    case ActionType.ChangeContract:
      return {
        ...state,
        output: DEFAULT_STATE.output,
        contract: action.payload
      };
  }
  return state;
};
