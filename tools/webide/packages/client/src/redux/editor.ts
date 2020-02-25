import { ActionType as ExamplesActionType, ChangeSelectedAction as ChangeSelectedExampleAction } from './examples';
import { Language } from './types';

export enum ActionType {
  ChangeLanguage = 'editor-change-language',
  ChangeCode = 'editor-change-code',
  ChangeDirty = 'editor-change-dirty',
  ChangeTitle = 'editor-change-title'
}

export interface EditorState {
  language: Language;
  code: string;
  title: string;
  dirty: boolean;
}

export class ChangeLanguageAction {
  public readonly type = ActionType.ChangeLanguage;
  constructor(public payload: EditorState['language']) {}
}

export class ChangeCodeAction {
  public readonly type = ActionType.ChangeCode;
  constructor(public payload: EditorState['code']) {}
}

export class ChangeDirtyAction {
  public readonly type = ActionType.ChangeDirty;
  constructor(public payload: EditorState['dirty']) {}
}

export class ChangeTitleAction {
  public readonly type = ActionType.ChangeTitle;
  constructor(public payload: EditorState['title']) {}
}

type Action =
  | ChangeCodeAction
  | ChangeLanguageAction
  | ChangeDirtyAction
  | ChangeTitleAction
  | ChangeSelectedExampleAction;

const DEFAULT_STATE: EditorState = {
  language: Language.CameLigo,
  code: '',
  title: '',
  dirty: false
};

export default (state = DEFAULT_STATE, action: Action): EditorState => {
  switch (action.type) {
    case ExamplesActionType.ChangeSelected:
      return {
        ...state,
        ...(!action.payload
          ? DEFAULT_STATE
          : { ...action.payload.editor, title: action.payload.name })
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
    case ActionType.ChangeDirty:
      return {
        ...state,
        dirty: action.payload
      };
    case ActionType.ChangeTitle:
      return {
        ...state,
        title: action.payload
      };
    default:
      return state;
  }
};
