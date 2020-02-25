import {
  ActionType as CompileActionType,
  ChangeEntrypointAction as ChangeCompileEntrypointAction,
  ChangeMichelsonFormatAction,
} from './compile';
import {
  ActionType as DeployActionType,
  ChangeEntrypointAction as ChangeDeployEntrypointAction,
  ChangeStorageAction as ChangeDeployStorageAction,
  UseTezBridgeAction,
} from './deploy';
import {
  ActionType as DryRunActionType,
  ChangeEntrypointAction as ChangeDryRunEntrypointAction,
  ChangeParametersAction as ChangeDryRunParametersAction,
  ChangeStorageAction as ChangeDryRunStorageAction,
} from './dry-run';
import { ActionType as EditorActionType, ChangeCodeAction, ChangeLanguageAction, ChangeTitleAction } from './editor';
import {
  ActionType as EvaluateFunctionActionType,
  ChangeEntrypointAction as ChangeEvaluateFunctionEntrypointAction,
  ChangeParametersAction as ChangeEvaluateFunctionParametersAction,
} from './evaluate-function';
import {
  ActionType as EvaluateValueActionType,
  ChangeEntrypointAction as ChangeEvaluateValueEntrypointAction,
} from './evaluate-value';
import { ActionType as ExamplesActionType, ChangeSelectedAction as ChangeSelectedExampleAction } from './examples';

export enum ActionType {
  ChangeShareLink = 'share-change-link'
}

export interface ShareState {
  link: string;
}

export class ChangeShareLinkAction {
  public readonly type = ActionType.ChangeShareLink;
  constructor(public payload: ShareState['link']) {}
}

type Action =
  | ChangeShareLinkAction
  | ChangeTitleAction
  | ChangeCodeAction
  | ChangeLanguageAction
  | ChangeCompileEntrypointAction
  | ChangeMichelsonFormatAction
  | ChangeDeployEntrypointAction
  | ChangeDeployStorageAction
  | UseTezBridgeAction
  | ChangeDryRunEntrypointAction
  | ChangeDryRunParametersAction
  | ChangeDryRunStorageAction
  | ChangeEvaluateFunctionEntrypointAction
  | ChangeEvaluateFunctionParametersAction
  | ChangeEvaluateValueEntrypointAction
  | ChangeSelectedExampleAction;

const DEFAULT_STATE: ShareState = {
  link: ''
};

export default (state = DEFAULT_STATE, action: Action): ShareState => {
  switch (action.type) {
    case EditorActionType.ChangeTitle:
    case ExamplesActionType.ChangeSelected:
    case EditorActionType.ChangeCode:
    case EditorActionType.ChangeLanguage:
    case CompileActionType.ChangeEntrypoint:
    case DeployActionType.ChangeEntrypoint:
    case DeployActionType.ChangeStorage:
    case DryRunActionType.ChangeEntrypoint:
    case DryRunActionType.ChangeParameters:
    case DryRunActionType.ChangeStorage:
    case EvaluateFunctionActionType.ChangeEntrypoint:
    case EvaluateFunctionActionType.ChangeParameters:
    case EvaluateValueActionType.ChangeEntrypoint:
      return {
        ...state,
        ...DEFAULT_STATE
      };
    case ActionType.ChangeShareLink:
      return {
        ...state,
        link: action.payload
      };
  }
  return state;
};
