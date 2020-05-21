import { Tool, ToolCommand } from './types';

export enum ActionType {
  ChangeTool = 'generate-deploy-script-change-tool',
  ChangeCommand = 'generate-deploy-script-change-command',
  ChangeEntrypoint = 'generate-deploy-script-change-entrypoint',
  ChangeStorage = 'generate-deploy-script-change-storage'
}

export interface GenerateDeployScriptState {
  tool: Tool;
  command: ToolCommand;
  entrypoint: string;
  originationAccount: string;
  storage: string;
  burnCap: number;
}

export class ChangeToolAction {
  public readonly type = ActionType.ChangeTool;
  constructor(public payload: GenerateDeployScriptState['tool']) {}
}

export class ChangeCommandAction {
  public readonly type = ActionType.ChangeCommand;
  constructor(public payload: GenerateDeployScriptState['command']) {}
}

export class ChangeEntrypointAction {
  public readonly type = ActionType.ChangeEntrypoint;
  constructor(public payload: GenerateDeployScriptState['entrypoint']) {}
}

export class ChangeStorageAction {
  public readonly type = ActionType.ChangeStorage;
  constructor(public payload: GenerateDeployScriptState['storage']) {}
}

type Action =
  | ChangeToolAction
  | ChangeCommandAction
  | ChangeEntrypointAction
  | ChangeStorageAction;

const DEFAULT_STATE: GenerateDeployScriptState = {
  tool: Tool.TezosClient,
  command: ToolCommand.Originate,
  entrypoint: '',
  storage: '',
  originationAccount: '',
  burnCap: 0
};

export default (
  state = DEFAULT_STATE,
  action: Action
): GenerateDeployScriptState => {
  switch (action.type) {
    case ActionType.ChangeTool:
      return {
        ...state,
        tool: action.payload
      };
    case ActionType.ChangeCommand:
      return {
        ...state,
        command: action.payload
      };
    case ActionType.ChangeEntrypoint:
      return {
        ...state,
        entrypoint: action.payload
      };
    case ActionType.ChangeStorage:
      return {
        ...state,
        storage: action.payload
      };
  }
  return state;
};
