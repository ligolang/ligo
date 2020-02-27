import { Command } from './types';

export enum ActionType {
  ChangeOutput = 'result-change-output',
  ChangeContract = 'result-change-contract'
}

export interface ResultState {
  command: Command;
  output: string;
  contract: string;
}

export class ChangeOutputAction {
  public readonly type = ActionType.ChangeOutput;
  constructor(
    public output: ResultState['output'],
    public command: ResultState['command']
  ) {}
}

export class ChangeContractAction {
  public readonly type = ActionType.ChangeContract;
  constructor(
    public contract: ResultState['contract'],
    public command: ResultState['command']
  ) {}
}

type Action = ChangeOutputAction | ChangeContractAction;

const DEFAULT_STATE: ResultState = {
  command: Command.Compile,
  output: '',
  contract: ''
};

export default (state = DEFAULT_STATE, action: Action): ResultState => {
  switch (action.type) {
    case ActionType.ChangeOutput:
      return {
        ...state,
        output: action.output,
        command: action.command
      };
    case ActionType.ChangeContract:
      return {
        ...state,
        output: DEFAULT_STATE.output,
        contract: action.contract,
        command: action.command
      };
  }
  return state;
};
