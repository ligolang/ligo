import { ActionType as ExamplesActionType, ChangeSelectedAction as ChangeSelectedExampleAction } from './examples';
import { Language } from './types';

export enum ActionType {
  ChangeLanguage = 'editor-change-language',
  ChangeCode = 'editor-change-code'
}

export interface EditorState {
  language: Language;
  code: string;
}

export class ChangeLanguageAction {
  public readonly type = ActionType.ChangeLanguage;
  constructor(public payload: EditorState['language']) {}
}

export class ChangeCodeAction {
  public readonly type = ActionType.ChangeCode;
  constructor(public payload: EditorState['code']) {}
}

type Action =
  | ChangeCodeAction
  | ChangeLanguageAction
  | ChangeSelectedExampleAction;

const DEFAULT_STATE: EditorState = {
  language: Language.CameLigo,
  code: ''
};

export default (state = DEFAULT_STATE, action: Action): EditorState => {
  switch (action.type) {
    case ExamplesActionType.ChangeSelected:
      return {
        ...state,
        ...(!action.payload ? DEFAULT_STATE : action.payload.editor)
      };
    case ActionType.ChangeLanguage:
      return {
        ...state,
        language: action.payload
      };
    case ActionType.ChangeCode:
      return {
        ...state,
        code: action.payload
      };
  }
  return state;
};
