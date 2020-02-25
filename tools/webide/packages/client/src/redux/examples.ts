import { ExampleState } from './example';

export enum ActionType {
  ChangeSelected = 'examples-change-selected',
  ClearSelected = 'examples-clear-selected'
}

export interface ExampleItem {
  id: string;
  name: string;
}

export interface ExamplesState {
  selected: ExampleState | null;
  list: ExampleItem[];
}

export class ChangeSelectedAction {
  public readonly type = ActionType.ChangeSelected;
  constructor(public payload: ExamplesState['selected']) {}
}

export class ClearSelectedAction {
  public readonly type = ActionType.ClearSelected;
}

type Action = ChangeSelectedAction | ClearSelectedAction;

export const DEFAULT_STATE: ExamplesState = {
  selected: null,
  list: []
};

if (process.env.NODE_ENV === 'development') {
  DEFAULT_STATE.list = [
    { id: 'MzkMQ1oiVHJqbcfUuVFKTw', name: 'CameLIGO Contract' },
    { id: 'FEb62HL7onjg1424eUsGSg', name: 'PascaLIGO Contract' },
    { id: 'JPhSOehj_2MFwRIlml0ymQ', name: 'ReasonLIGO Contract' }
  ];
}

export default (state = DEFAULT_STATE, action: Action): ExamplesState => {
  switch (action.type) {
    case ActionType.ChangeSelected:
      return {
        ...state,
        selected: action.payload
      };
    case ActionType.ClearSelected:
      return {
        ...state,
        selected: null
      };
    default:
      return state;
  }
};
