import { ExampleState } from './example';

export enum ActionType {
  ChangeSelected = 'examples-change-selected'
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

type Action = ChangeSelectedAction;

export const DEFAULT_STATE: ExamplesState = {
  selected: null,
  list: []
};

export default (state = DEFAULT_STATE, action: Action): ExamplesState => {
  switch (action.type) {
    case ActionType.ChangeSelected:
      return {
        ...state,
        selected: action.payload
      };
  }
  return state;
};
