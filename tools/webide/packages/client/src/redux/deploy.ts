import { ActionType as ExamplesActionType, ChangeSelectedAction as ChangeSelectedExampleAction } from './examples';

export enum ActionType {
  ChangeEntrypoint = 'deploy-change-entrypoint',
  ChangeStorage = 'deploy-change-storage',
  UseTezBridge = 'deploy-use-tezbridge'
}

export interface DeployState {
  entrypoint: string;
  storage: string;
  useTezBridge: boolean;
}

export class ChangeEntrypointAction {
  public readonly type = ActionType.ChangeEntrypoint;
  constructor(public payload: DeployState['entrypoint']) {}
}

export class ChangeStorageAction {
  public readonly type = ActionType.ChangeStorage;
  constructor(public payload: DeployState['storage']) {}
}

export class UseTezBridgeAction {
  public readonly type = ActionType.UseTezBridge;
  constructor(public payload: DeployState['useTezBridge']) {}
}

type Action =
  | ChangeEntrypointAction
  | ChangeStorageAction
  | UseTezBridgeAction
  | ChangeSelectedExampleAction;

const DEFAULT_STATE: DeployState = {
  entrypoint: '',
  storage: '',
  useTezBridge: false
};

export default (state = DEFAULT_STATE, action: Action): DeployState => {
  switch (action.type) {
    case ExamplesActionType.ChangeSelected:
      return {
        ...state,
        ...(!action.payload ? DEFAULT_STATE : action.payload.deploy)
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
    case ActionType.UseTezBridge:
      return {
        ...state,
        useTezBridge: action.payload
      };
  }
  return state;
};
