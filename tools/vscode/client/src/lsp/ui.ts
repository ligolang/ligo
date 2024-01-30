import * as vscode from 'vscode'
import { isDefined, Maybe } from '../common/base';
import { LigoContext } from '../common/LigoContext';
import * as ex from '../common/exceptions'

// All keys are expected to be in kebab-case.
export type LspRememberingKey
  = "compile-contract"
  | "storage-entrypoint"
  | "storage-expression"
  | "dry-run-entrypoint"
  | "dry-run-parameter"
  | "dry-run-storage"
  | "call-expression"
  | "call-arg"
  | "call-value"

function getDefaultValue(key: LspRememberingKey): string {
  switch (key) {
    case "call-expression":
    case "call-value":
      return "main";

    case "compile-contract":
    case "storage-entrypoint":
    case "storage-expression":
    case "dry-run-entrypoint":
    case "dry-run-parameter":
    case "dry-run-storage":
    case "call-arg":
      return "";
  }
}

function splitKey(key: LspRememberingKey): string[] {
  return key.split('-');
}

export type InputBoxOptions = {
  title: string,
  placeHolder: string,
  rememberingKey: LspRememberingKey,
}

export async function createRememberingInputBox(context: LigoContext, options: InputBoxOptions): Promise<string> {
  const rememberedVal = context.workspaceState.lspRememberedValue(...splitKey(options.rememberingKey));

  const res = await vscode.window.showInputBox({
    title: options.title,
    placeHolder: options.placeHolder,
    value: isDefined(rememberedVal.value) ? rememberedVal.value : getDefaultValue(options.rememberingKey),
    ignoreFocusOut: true,
  }).then((newVal) => {
    if (isDefined(newVal)) {
      rememberedVal.value = newVal;
    }
    return newVal
  });
  if (!isDefined(res)) {
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
