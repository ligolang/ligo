import { Tool, ToolCommand } from './types';

export enum ActionType {
  ChangeTool = 'generate-command-change-tool',
  ChangeCommand = 'generate-command-change-command',
  ChangeEntrypoint = 'generate-command-change-entrypoint',
  ChangeStorage = 'generate-command-change-storage'
}

export interface GenerateCommandState {
  tool: Tool;
  command: ToolCommand;
  entrypoint: string;
  originationAccount: string;
  storage: string;
  burnCap: number;
}

export class ChangeToolAction {
  public readonly type = ActionType.ChangeTool;
  constructor(public payload: GenerateCommandState['tool']) {}
}

export class ChangeCommandAction {
  public readonly type = ActionType.ChangeCommand;
  constructor(public payload: GenerateCommandState['command']) {}
}

export class ChangeEntrypointAction {
  public readonly type = ActionType.ChangeEntrypoint;
  constructor(public payload: GenerateCommandState['entrypoint']) {}
}

export class ChangeStorageAction {
  public readonly type = ActionType.ChangeStorage;
  constructor(public payload: GenerateCommandState['storage']) {}
}

type Action =
  | ChangeToolAction
  | ChangeCommandAction
  | ChangeEntrypointAction
  | ChangeStorageAction;

const DEFAULT_STATE: GenerateCommandState = {
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
): GenerateCommandState => {
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
