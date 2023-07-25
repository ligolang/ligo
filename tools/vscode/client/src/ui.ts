import * as vscode from 'vscode'
import { Maybe } from './common'
import * as ex from './exceptions'

const extensionState = new Map<string, Maybe<string>>();
export function initializeExtensionState() {
  // Compile contract
  extensionState.set('compile-contract', undefined);
  extensionState.set('compile-format', undefined);
  // Compile storage
  extensionState.set('storage-entrypoint', undefined);
  extensionState.set('storage-expression', undefined);
  // Compile expression
  extensionState.set('compile-expression', undefined);
  extensionState.set('syntax', undefined);
  // Dry-run
  extensionState.set('dry-run-entrypoint', undefined);
  extensionState.set('dry-run-parameter', undefined);
  extensionState.set('dry-run-storage', undefined);
  // Evaluate function
  extensionState.set('call-expression', undefined);
  // Evaluate Value
  extensionState.set('call-arg', undefined);
  extensionState.set('call-value', undefined);
  // Deploy
  extensionState.set('network', undefined)
}

export type InputBoxOptions = {
  title: string,
  placeHolder: string,
  rememberingKey: string,
  defaultValue: string,
}

export async function createRememberingInputBox(options: InputBoxOptions): Promise<string> {
  const res = await vscode.window.showInputBox({
    title: options.title,
    placeHolder: options.placeHolder,
    value: extensionState.get(options.rememberingKey) || (options.defaultValue || ''),
    ignoreFocusOut: true,
  }).then((newVal) => {
    if (newVal !== undefined) {
      extensionState.set(options.rememberingKey, newVal);
    }
    return newVal
  });
  if (!res) {
    throw new ex.UserInterruptionException()
  }
  return res;
}

export async function createQuickPickBox(
  listOptions: readonly string[],
  title: string,
  placeHolder: string,
): Promise<string> {
  const res = await vscode.window.showQuickPick(
    listOptions,
    {
      title,
      placeHolder,
      ignoreFocusOut: true,
      canPickMany: false,
    },
  );

  if (!res) {
    throw new ex.UserInterruptionException()
  }
  return res
}
