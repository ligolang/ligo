import { NetworkType } from '@airgap/beacon-sdk';
import { protocolType } from './compile';

import {
  ActionType as ExamplesActionType,
  ChangeSelectedAction as ChangeSelectedExampleAction,
} from './examples';

export enum networkType {
  Mainnet = 'mainnet',
  Jakartanet = 'jakartanet',
  Kathmandunet = 'kathmandunet'
}

export enum signerType {
  Beacon = 'becon',
  Tezbridge = 'tezbridge',
  Sign = 'sign',
}

export enum ActionType {
  ChangeEntrypoint = 'deploy-change-entrypoint',
  ChangeStorage = 'deploy-change-storage',
  ChangeProtocol = 'deploy-change-protocol',
  UseNetwork = 'deploy-network',
  UseSigner = 'deploy-signer',
}

export interface DeployState {
  entrypoint: string;
  storage: string;
  network: string;
  protocol: string;
  signer: string;
}

export class ChangeEntrypointAction {
  public readonly type = ActionType.ChangeEntrypoint;
  constructor(public payload: DeployState['entrypoint']) {}
}

export class ChangeStorageAction {
  public readonly type = ActionType.ChangeStorage;
  constructor(public payload: DeployState['storage']) {}
}

export class ChangeProtocolAction {
  public readonly type = ActionType.ChangeProtocol;
  constructor(public payload: DeployState['protocol']) {}
}

export class UseNetworkAction {
  public readonly type = ActionType.UseNetwork;
  constructor(public payload: DeployState['network']) {}
}

export class UseSignerAction {
  public readonly type = ActionType.UseSigner;
  constructor(public payload: DeployState['signer']) {}
}

type Action =
  | ChangeEntrypointAction
  | ChangeStorageAction
  | ChangeSelectedExampleAction
  | UseNetworkAction
  | UseSignerAction
  | ChangeProtocolAction;

const DEFAULT_STATE: DeployState = {
  entrypoint: '',
  storage: '',
  network: NetworkType.JAKARTANET,
  protocol: protocolType.Jakarta,
  signer: signerType.Sign,
};

const deploy = (state = DEFAULT_STATE, action: Action): DeployState => {
  switch (action.type) {
    case ExamplesActionType.ChangeSelected:
      return {
        ...state,
        ...(!action.payload ? DEFAULT_STATE : action.payload.deploy),
      };
    case ActionType.ChangeEntrypoint:
      return {
        ...state,
        entrypoint: action.payload,
      };
    case ActionType.ChangeStorage:
      return {
        ...state,
        storage: action.payload,
      };
    case ActionType.ChangeProtocol:
      return {
        ...state,
        protocol: action.payload,
      };
    case ActionType.UseNetwork:
      return {
        ...state,
        network: action.payload,
      };
    case ActionType.UseSigner:
      return {
        ...state,
        signer: action.payload,
      };

    default:
      return state;
  }
};

export default deploy;
