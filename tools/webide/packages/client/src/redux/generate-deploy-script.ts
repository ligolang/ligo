import {
  ActionType as ExamplesActionType,
  ChangeSelectedAction as ChangeSelectedExampleAction,
} from './examples';
import { Tool } from './types';

export enum ActionType {
  ChangeTool = 'generate-deploy-script-change-tool',
  ChangeEntrypoint = 'generate-deploy-script-change-entrypoint',
  ChangeProtocol = 'generate-deploy-script-change-protocol',
  ChangeStorage = 'generate-deploy-script-change-storage',
}

export interface GenerateDeployScriptState {
  tool: Tool;
  entrypoint: string;
  originationAccount: string;
  storage: string;
  burnCap: number;
  protocol: string;
}

export class ChangeToolAction {
  public readonly type = ActionType.ChangeTool;
  constructor(public payload: GenerateDeployScriptState['tool']) {}
}

export class ChangeEntrypointAction {
  public readonly type = ActionType.ChangeEntrypoint;
  constructor(public payload: GenerateDeployScriptState['entrypoint']) {}
}

export class ChangeProtocolAction {
  public readonly type = ActionType.ChangeProtocol;
  constructor(public payload: GenerateDeployScriptState['protocol']) {}
}

export class ChangeStorageAction {
  public readonly type = ActionType.ChangeStorage;
  constructor(public payload: GenerateDeployScriptState['storage']) {}
}

type Action =
  | ChangeToolAction
  | ChangeEntrypointAction
  | ChangeStorageAction
  | ChangeProtocolAction
  | ChangeSelectedExampleAction;

const DEFAULT_STATE: GenerateDeployScriptState = {
  tool: Tool.TezosClient,
  entrypoint: '',
  storage: '',
  originationAccount: '',
  protocol: 'kathmandu',
  burnCap: 0,
};

const generateDeployScript = (
  state = DEFAULT_STATE,
  action: Action
): GenerateDeployScriptState => {
  switch (action.type) {
    case ExamplesActionType.ChangeSelected:
      return {
        ...state,
        ...(!action.payload
          ? DEFAULT_STATE
          : action.payload.generateDeployScript),
      };
    case ActionType.ChangeTool:
      return {
        ...state,
        tool: action.payload,
      };
    case ActionType.ChangeEntrypoint:
      return {
        ...state,
        entrypoint: action.payload,
     };
    case ActionType.ChangeProtocol:
      return {
        ...state,
        protocol: action.payload,
      };
    case ActionType.ChangeStorage:
      return {
        ...state,
        storage: action.payload,
      };
    default:
      return state;
  }
};

export default generateDeployScript;
