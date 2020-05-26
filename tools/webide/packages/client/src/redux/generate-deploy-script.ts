import { ActionType as ExamplesActionType, ChangeSelectedAction as ChangeSelectedExampleAction } from './examples';
<<<<<<< HEAD:tools/webide/packages/client/src/redux/generate-deploy-script.ts
import { Tool } from './types';

export enum ActionType {
  ChangeTool = 'generate-deploy-script-change-tool',
=======
import { Tool, ToolCommand } from './types';

export enum ActionType {
  ChangeTool = 'generate-deploy-script-change-tool',
  ChangeCommand = 'generate-deploy-script-change-command',
>>>>>>> bc2d95d6f6dd4959097e5b6cdb19bce0cc6b3c01:tools/webide/packages/client/src/redux/generate-command.ts
  ChangeEntrypoint = 'generate-deploy-script-change-entrypoint',
  ChangeStorage = 'generate-deploy-script-change-storage'
}

export interface GenerateDeployScriptState {
  tool: Tool;
  entrypoint: string;
  originationAccount: string;
  storage: string;
  burnCap: number;
}

export class ChangeToolAction {
  public readonly type = ActionType.ChangeTool;
  constructor(public payload: GenerateDeployScriptState['tool']) {}
<<<<<<< HEAD:tools/webide/packages/client/src/redux/generate-deploy-script.ts
=======
}

export class ChangeCommandAction {
  public readonly type = ActionType.ChangeCommand;
  constructor(public payload: GenerateDeployScriptState['command']) {}
>>>>>>> bc2d95d6f6dd4959097e5b6cdb19bce0cc6b3c01:tools/webide/packages/client/src/redux/generate-command.ts
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
  | ChangeEntrypointAction
  | ChangeStorageAction
  | ChangeSelectedExampleAction;

const DEFAULT_STATE: GenerateDeployScriptState = {
  tool: Tool.TezosClient,
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
    case ExamplesActionType.ChangeSelected:
<<<<<<< HEAD:tools/webide/packages/client/src/redux/generate-deploy-script.ts
=======
      return {
        ...state,
        ...(!action.payload ? DEFAULT_STATE : action.payload.generateDeployScript)
      };
    case ActionType.ChangeTool:
>>>>>>> bc2d95d6f6dd4959097e5b6cdb19bce0cc6b3c01:tools/webide/packages/client/src/redux/generate-command.ts
      return {
        ...state,
        ...(!action.payload ? DEFAULT_STATE : action.payload.generateDeployScript)
      };
    case ActionType.ChangeTool:
      return {
        ...state,
        tool: action.payload
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
