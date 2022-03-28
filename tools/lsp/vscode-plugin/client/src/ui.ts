import * as vscode from 'vscode'

export type Maybe<T> = T | undefined

const extensionState = new Map<string, Maybe<string>>();
export function initializeExtensionState() {
  extensionState.set('compile-contract', undefined);
  extensionState.set('compile-expression', undefined);
  extensionState.set('syntax', undefined);
  extensionState.set('dry-run-entrypoint', undefined);
  extensionState.set('dry-run-parameter', undefined);
  extensionState.set('dry-run-storage', undefined);
  extensionState.set('call-expression', undefined);
  extensionState.set('call-arg', undefined);
  extensionState.set('call-value', undefined);
}

export type InputBoxOptions = {
  title: string,
  placeHolder: string,
  rememberingKey: string,
  defaultValue: string,
}

export function createRememberingInputBox(options: InputBoxOptions): Thenable<string | undefined> {
  return vscode.window.showInputBox({
    title: options.title,
    placeHolder: options.placeHolder,
    value: extensionState.get(options.rememberingKey) || (options.defaultValue || ''),
    ignoreFocusOut: true,
  }).then((newVal) => {
    extensionState.set(options.rememberingKey, newVal);
    return newVal
  });
}

export function createQuickPickBox(
  listOptions: readonly string[],
  title: string,
  placeHolder: string,
): Thenable<string | undefined> {
  return vscode.window.showQuickPick(
    listOptions,
    {
      title,
      placeHolder,
      ignoreFocusOut: true,
      canPickMany: false,
    },
  );
}
