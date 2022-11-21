import { ActionType as ExamplesActionType, ChangeSelectedAction as ChangeSelectedExampleAction } from './examples';

export enum MichelsonFormat {
  Text = 'text',
  Json = 'json'
}

export enum protocolType {
  Lima = 'lima',
  Kathmandu = 'kathmandu'
}

export enum ActionType {
  ChangeEntrypoint = 'compile-change-entrypoint',
  ChangeProtocol = 'compile-change-protocol',
  ChangeMichelsonFormat = 'compile-change-michelson-format'
}

export interface CompileState {
  entrypoint: string;
  michelsonFormat?: MichelsonFormat;
  protocol: string;
}

export class ChangeEntrypointAction {
  public readonly type = ActionType.ChangeEntrypoint;
  constructor(public payload: CompileState['entrypoint']) {}
}

export class ChangeProtocolAction {
  public readonly type = ActionType.ChangeProtocol;
  constructor(public payload: CompileState['protocol']) {}
}

export class ChangeMichelsonFormatAction {
  public readonly type = ActionType.ChangeMichelsonFormat;
  constructor(public payload: CompileState['michelsonFormat']) {}
}

type Action =
  | ChangeEntrypointAction
  | ChangeMichelsonFormatAction
  | ChangeSelectedExampleAction
  | ChangeProtocolAction;

const DEFAULT_STATE: CompileState = {
  entrypoint: '',
  protocol: 'kathmandu',
  michelsonFormat: MichelsonFormat.Text
};

const compile = (state = DEFAULT_STATE, action: Action): CompileState => {
  switch (action.type) {
    case ExamplesActionType.ChangeSelected:
      return {
        ...state,
        ...(!action.payload ? DEFAULT_STATE : action.payload.compile)
      };
    case ActionType.ChangeEntrypoint:
      return {
        ...state,
        entrypoint: action.payload
      };
    case ActionType.ChangeProtocol:
      return {
        ...state,
        protocol: action.payload
      };
    case ActionType.ChangeMichelsonFormat:
      return {
        ...state,
        michelsonFormat: action.payload
      };
    default:
      return state;
  }
};

export default compile
