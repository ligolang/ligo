import * as vscode from 'vscode'
import { isDefined } from '../common/base';
import { LigoContext } from '../common/LigoContext';
import * as ex from '../common/exceptions'

/**
 * A key used for remembering quick picks/input boxes. All keys are expected to
 * be in kebab-case.
 */
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

/**
 * Depending on the key, we can provide a reasonable default value to the user,
 * which will be determined by this function.
 */
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

/** Splits a key by hyphens (-). */
function splitKey(key: LspRememberingKey): string[] {
  return key.split('-');
}

/** Helper type holding data used to display an input box to the user. */
export type InputBoxOptions = {
  /** The title of the input box. */
  title: string,

  /** A string which will be shown while no input is provided. */
  placeHolder: string,

  /** The key used for remembering the last provided value to this input box. */
  rememberingKey: LspRememberingKey,
}

/**
 * Creates an input box that will store the last input in the VSCode storage.
 *
 * @returns A promise resolving to the inputted value.
 * @throws ex.UserInterruptionException If the user interrupted this box.
 */
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

/**
 * Creates a quick pick box.
 *
 * @param listOptions The options in which the user will be able to choose from.
 * @param title The title of the input box.
 * @param placeHolder The key used for remembering the last provided value to
 * this input box.
 * @returns A promise resolving to the inputted value.
 * @throws ex.UserInterruptionException If the user interrupted this box.
 */
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
