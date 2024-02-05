// SPDX-FileCopyrightText: 2020 Tocqueville Group
// SPDX-License-Identifier: LicenseRef-MIT-TQ
//
// SPDX-FileCopyrightText: 2022 Oxhead Alpha
// SPDX-License-Identifier: LicenseRef-MIT-OA
//
// SPDX-FileCopyrightText: 2022 Microsoft
// SPDX-License-Identifier: LicenseRef-MIT-Microsoft

// Useful UI elements which is used to make the plugin more pleasant to use.

import path from 'path';
import * as vscode from 'vscode';
import { QuickPickItem } from 'vscode';
import { ContractMetadata, impossible, getCurrentWorkspacePath } from './base'
import { LigoContext, ValueAccess } from '../common/LigoContext'
import * as fs from 'fs'
import { MultiStepInput } from '../common/ui';
import { InputBoxType, InputValueLang, isDefined, Maybe, Ref } from '../common/base';
import * as ex from '../common/exceptions'

export type SteppingGranularity
  = 'statement'
  | 'expression'
  | 'expressionSurrounded'

/**
 * Keeps information about stepping granularity status.
 *
 * In UI this creates a button & command for changing the used granularity,
 * plus it can also be manually changed via the `status` setter.
 *
 * It is then possible to read the status from the same 'status' field,
 * or subscribe on changes in the constructor.
 */
export class DebugSteppingGranularityStatus implements vscode.Disposable {
  private bar: vscode.StatusBarItem;
  // @ts-ignore uninitialized (set in smart setter)
  private _status: SteppingGranularity;
  private lastStatus: Maybe<SteppingGranularity>
  private readonly statusChangeEvent = new vscode.EventEmitter<SteppingGranularity>();
  private disposables: vscode.Disposable[] = new Array();

  constructor
    (onStatusChanged?: (newGranularity: SteppingGranularity) => Promise<void>
    ) {
    // We will create a `StatusBarItem` and change stepping granularity
    // through it.

    const switchCommand = 'extension.ligo-debugger.switchSteppingGranularity';
    const useLastCommand = 'extension.ligo-debugger.useLastSteppingGranularity';

    // Note: VSCode has built-in status bar that is added on debug session start,
    // At the moment of writing it has id='status.debug' and priority=30, and
    // our status bar we want to put alongside.
    this.bar = vscode.window.createStatusBarItem
      ('status.debug.ligo.stepping'
        , vscode.StatusBarAlignment.Left
        , 28
      );
    this.bar.name = 'Debug Stepping Granularity Status';
    this.bar.tooltip = 'Select debug step granularity';
    this.bar.command = switchCommand;

    this.status = 'statement';

    if (onStatusChanged) {
      this.disposables.push(
        this.statusChangeEvent.event(onStatusChanged)
      );
    }

    this.disposables.push(
      vscode.commands.registerCommand(switchCommand, async () => {
        const newGranularity = await this.createStatusChoosingQuickPick();
        if (newGranularity) {
          this.status = newGranularity;
        }
      }),
      vscode.commands.registerCommand(useLastCommand, () =>
        this.toLastStatus()
      )
    );

  }

  /**
   * Smart field that keeps the current 'SteppingGranularity', automatically
   * updating the UI if assigned a new value.
   */
  get status(): SteppingGranularity {
    return this._status;
  }
  set status(newStatus: SteppingGranularity) {
    if (this._status != newStatus) {
      // We update the last used status - but only if an actual status change
      // is performed, the other UX is annoying.
      this.lastStatus = this._status;
    }

    this._status = newStatus;
    this.bar.text =
      `$(debug-step-over) ${DebugSteppingGranularityStatus.granularityToUIString(newStatus)}`;

    this.statusChangeEvent.fire(newStatus);
    // Note ↑↑: there are chances that, assuming we send a request in
    // the callback, the backend won't receive the messages in FIFO order.
    // If this is ever found to be the case, one option is to simply
    // attach a counter to each status update and let backend accept only
    // the latest message.
  }

  /**
   * Switch to the previously selected status.
   *
   * Does nothing and does not trigger any events if the status was never
   * changed.
   */
  public toLastStatus() {
    if (this.lastStatus) {
      this.status = this.lastStatus
      // this.lastStatus is updated in the smart setter
    }
  }

  /**
   * Display the button in UI.
   */
  public show() { this.bar.show(); }

  /**
   * Hide the button in UI.
   */
  public hide() { this.bar.hide(); }

  public dispose() {
    this.bar.dispose();
    this.disposables.forEach(d => d.dispose());
  }

  // Creates a quick pick for stepping granularity selection and returns
  // the selected option.
  private async createStatusChoosingQuickPick(): Promise<Maybe<SteppingGranularity>> {
    const allOptions: SteppingGranularity[] = ["statement", "expression", "expressionSurrounded"]
    const counter = { value: 0 };
    const pickOptions = allOptions.map(granularity => {
      counter.value++;
      var res: QuickPickItem & { type: SteppingGranularity } = {
        type: granularity,
        // We add a counter to the name to make it easy to select the necessary option
        // by typing N and hitting Enter
        label: `${counter.value}. ${DebugSteppingGranularityStatus.granularityToUIString(granularity)}`,
      }
      switch (granularity) {
        case "statement":
          res.description = 'Stop only at statements'
          break;
        case "expression":
          res.description = 'Stop at statements + after each evaluated expression'
          break;
        case "expressionSurrounded":
          res.description = 'Stop at statements + before/after each expression evaluation'
          break;
        // If you need to add a case here, probably you also want to update
        // `allOptions` variable above.
        default:
          impossible(granularity);
      }
      return res;
    });
    var chosen = await vscode.window.showQuickPick(pickOptions, {
      placeHolder: "Select granularity for debug stepping",
    });
    return chosen?.type;
  }

  static granularityToUIString(granularity: SteppingGranularity): string {
    switch (granularity) {
      case 'statement':
        return "Statement";
      case 'expression':
        return "Expression";
      case 'expressionSurrounded':
        return "Expression (pre + post)";
    }
  }
}

const suggestTypeValue = (mitype: string): { value: string, selection?: [number, number] } => {
  const startsWith = (prefix: string, str: string): boolean =>
    str.substring(0, prefix.length) === prefix

  // Some of these assume that supplying Michelson values is possible.
  if (mitype === "string") {
    return { value: "\"\"", selection: [1, 1] }
  }
  else if (mitype === "int" || mitype === "nat" || mitype === "mutez") {
    return { value: "0" }
  }
  else if (mitype === "bytes") {
    // We select 0x, not put the cursor at the end, because the user will likely
    // copypaste a ready value from somewhere else.
    return { value: "0x" }
  }
  else if (mitype === "timestamp") {
    let defTime = new Date().toISOString()
    return { value: "\"" + defTime + "\"", selection: [1, defTime.length + 1] }
  }
  else if (startsWith("option", mitype)) {
    return { value: "michelson:None" }
  }
  else {
    return { value: "" }
  }
}

const oldValueSelection = (mitype: string, oldVal: string): Maybe<[number, number]> => {
  const startsWith = (prefix: string, str: string): boolean =>
    str.substring(0, prefix.length) === prefix
  const len = oldVal.length

  if (oldVal.charAt(0) == "\"" && (mitype === "string" || mitype === "timestamp" || mitype === "address")) {
    return [1, len - 1]
  }
  else if ((oldVal.charAt(0) == "{" || oldVal.charAt(0) == "[")
    && (startsWith("list", mitype) || startsWith("set", mitype) || startsWith("map", mitype) || startsWith("big_map", mitype))) {
    return [1, len - 1]
  }
}

// Create QuickPick which remembers previously
// inputted value in workspace storage.
export async function createRememberingQuickPick(
  contractMetadata: ContractMetadata,
  placeHolder: string
): Promise<Maybe<string>> {

  const currentFilePath = vscode.window.activeTextEditor?.document.uri.fsPath
  const entrypoints = currentFilePath && contractMetadata.entrypoints
  const pickerOptions: QuickPickItem[] =
    entrypoints ?
      Object.entries(entrypoints).map(([name, _michelsonType]) => {
        return {
          label: name,
          description: ""
        }
      }) :
      []

  const quickpick =
    pickerOptions.length == 1
      ? { then: (pass) => pass(pickerOptions[0]) }
      : vscode.window.showQuickPick<QuickPickItem>
        (pickerOptions,
          {
            placeHolder,
            ignoreFocusOut: true
          }
        )

  return quickpick.then(newVal => {
    if (newVal && newVal.label === "default") {
      newVal.label = "";
    }
    return newVal?.label;
  })
}

export async function getConfigPath(context: LigoContext): Promise<Maybe<string>> {
  interface State {
    pickedConfigPath?: string;
  }

  async function askForConfigPath(input: MultiStepInput<State>, state: Ref<State>) {
    const rememberedValue = context.workspaceState.lastConfigPath();

    let lastValue = "";
    if (isDefined(rememberedValue.value)) {
      lastValue = rememberedValue.value;
    }

    const result = await input.showInputBox({
      placeholder: "Config path",
      totalSteps: 1,
      value: lastValue,
      prompt: "Write a path to LIGO config file (or just close this input box if you want to use launch.json config)",
      validate: async (configPath) => {
        if (!fs.existsSync(configPath)) {
          return "Config file doesn't exist";
        } else {
          const extension = path.extname(configPath);
          switch (extension) {
            case ".mligo":
            case ".jsligo":
              break;
            default:
              return `Expected .mligo or .jsligo extension. Got: ${extension}`;
          }
        }
      },
      ignoreFocusOut: true
    });

    rememberedValue.value = result;

    state.ref.pickedConfigPath = result;
  }

  const result: State =
    await MultiStepInput.run((input, state) => askForConfigPath(input, state), {});

  return result.pickedConfigPath;
}

export async function getModuleName(
  context: LigoContext,
  validateModuleName: (entrypoint: string) => Promise<Maybe<string>>,
  moduleNamesList: [string, string][]
): Promise<Maybe<string>> {

  interface State {
    pickedModuleName?: string;
  }

  async function askForModuleName(input: MultiStepInput<State>, state: Ref<State>) {
    type MarkedQuickPickItem = { realName: string } & QuickPickItem;

    const moduleNames: MarkedQuickPickItem[] = moduleNamesList.map(([modName, pretty]) => ({ realName: modName, label: pretty }));

    const remembered = context.workspaceState.lastModuleName();

    if (moduleNames.length <= 1) {
      if (moduleNames.length === 0) {
        var msg = "Given contract doesn't have any modules."

        throw new Error(msg);
      }
      state.ref.pickedModuleName = moduleNames[0].realName;
      return;
    }

    let activeItem: Maybe<MarkedQuickPickItem>;
    if (isDefined(remembered.value)) {
      for (let moduleName of moduleNames) {
        if (moduleName.label === remembered.value) {
          activeItem = moduleName;
          break;
        }
      }
    }

    const pick: MarkedQuickPickItem = await input.showQuickPick({
      totalSteps: 1,
      items: moduleNames,
      activeItem,
      placeholder: "Choose a module name to run"
    });

    const validateResult = await validateModuleName(pick.realName);
    if (validateResult) {
      vscode.window.showWarningMessage(validateResult);
      input.doNotRecordStep();
      return (input: MultiStepInput<State>, state: Ref<State>) => askForModuleName(input, state);
    } else {
      remembered.value = pick.label;
      state.ref.pickedModuleName = pick.realName;
    }
  }

  const result: State =
    await MultiStepInput.run(
      (input: MultiStepInput<State>, state: Ref<State>) => askForModuleName(input, state), {}
    );

  return result.pickedModuleName;
}

export async function getParameterOrStorage(
  context: LigoContext,
  validateInput: (inputType: InputBoxType, valueLang: InputValueLang) => (value: string) => Promise<Maybe<string>>,
  inputBoxType: InputBoxType,
  placeHolder: string,
  prompt: string,
  moduleName: string,
  contractMetadata: ContractMetadata,
  entrypoint: string,
  showSwitchButton = true,
): Promise<[string, InputValueLang]> {

  const totalSteps = 1;

  let rememberedVal: ValueAccess<[string, InputValueLang]>;
  let michelsonType: string;
  switch (inputBoxType) {
    case "parameter":
      // Consider picked entrypoint in the key to remember value depending on an entrypoint
      rememberedVal = context.workspaceState.lastParameterOrStorageValue(inputBoxType, moduleName, entrypoint);
      michelsonType = contractMetadata.parameterMichelsonType;
      break;
    case "storage":
      rememberedVal = context.workspaceState.lastParameterOrStorageValue(inputBoxType, moduleName, entrypoint);
      michelsonType = contractMetadata.storageMichelsonType;
      break;
  }

  // These null checks is just a legacy. We can put them
  // into the context before by the accident and now we don't want to fail.
  if (!isDefined(rememberedVal.value) || rememberedVal.value[0] == null || rememberedVal.value[1] == null) {
    rememberedVal.value = [suggestTypeValue(michelsonType).value, "LIGO"];
  }

  class SwitchButton implements vscode.QuickInputButton {
    public lang: InputValueLang;
    public tooltip: string;

    constructor(public iconPath: vscode.Uri, lang: InputValueLang) {
      this.lang = lang;
      this.tooltip = `Input in ${lang} format`;
    }

    static readonly LigoSwitch = new SwitchButton(
      vscode.Uri.file(context.asAbsolutePath('resources/ligo.png')),
      "LIGO"
    );

    static readonly MichelsonSwitch = new SwitchButton(
      vscode.Uri.file(context.asAbsolutePath('resources/tezos.png')),
      "Michelson"
    );
  }

  interface State {
    value?: string;
    currentSwitch: SwitchButton;
  }

  async function askValue(input: MultiStepInput<State>, state: Ref<State>) {
    // This check seems redundant but without it
    // `tsc` will coplain about `rememberVal.value may be undefined`.
    if (!isDefined(rememberedVal.value)) {
      return;
    }

    let placeholderExtra: string = ''
    if (inputBoxType == "parameter" && entrypoint) {
      placeholderExtra = " for '" + entrypoint + "' entrypoint";
    }

    const [previousVal, _] = rememberedVal.value
    const defaultValue = { value: previousVal, selection: oldValueSelection(michelsonType, previousVal) }

    let valueLang: InputValueLang, buttons: SwitchButton[];
    if (showSwitchButton) {
      valueLang = state.ref.currentSwitch.lang;
      buttons = [state.ref.currentSwitch];
    } else {
      valueLang = "LIGO";
      buttons = [];
    }

    // Unfortunately, we can't use 'valueSelection' in low-level 'createInputBox'.
    // (https://github.com/microsoft/vscode/issues/56759)
    const pick = await input.showInputBox({
      totalSteps,
      placeholder: placeHolder + placeholderExtra,
      prompt,
      value: defaultValue.value,
      validate: validateInput(inputBoxType, valueLang),
      buttons,
      // Keep input box open if focus is moved out
      ignoreFocusOut: true
    });

    const currentValue = input.getCurrentValue();
    if (isDefined(currentValue)) {
      rememberedVal.value[0] = currentValue;
    }

    input.doNotRecordStep();

    if (pick instanceof SwitchButton) {
      switch (pick) {
        case SwitchButton.LigoSwitch:
          state.ref.currentSwitch = SwitchButton.MichelsonSwitch;
          break;
        case SwitchButton.MichelsonSwitch:
          state.ref.currentSwitch = SwitchButton.LigoSwitch;
          break;
      }

      return (input: MultiStepInput<State>, state: Ref<State>) => askValue(input, state);
    } else {
      state.ref.value = pick;
    }
  }

  let switchButton: SwitchButton;
  switch (rememberedVal.value[1]) {
    case "LIGO":
      switchButton = SwitchButton.LigoSwitch;
      break;
    case "Michelson":
      switchButton = SwitchButton.MichelsonSwitch;
      break;
  }

  const result: State =
    await MultiStepInput.run(
      (input: MultiStepInput<State>, state) => askValue(input, state),
      { currentSwitch: switchButton }
    );

  if (isDefined(result.value) && isDefined(result.currentSwitch.lang)) {
    rememberedVal.value = [result.value, result.currentSwitch.lang];
    return [result.value, result.currentSwitch.lang];
  } else {
    throw new ex.UserInterruptionException()
  }
}

export async function createConfigSnippet(context: LigoContext): Promise<boolean> {
  return vscode.window.showSaveDialog({
    defaultUri: getCurrentWorkspacePath(),
    filters: {
      LIGO: ['jsligo', 'mligo']
    },
  }).then(async pickedFile => {
    if (isDefined(pickedFile)) {
      const lang = path.extname(pickedFile.fsPath);

      switch (lang) {
        case ".mligo":
        case ".jsligo":
          break;
        default:
          let actualLang = lang == "" ? "<none>" : lang;
          const errorMessage = [
            "Expected a file to have an extension .mligo or .jsligo",
            `Got: ${actualLang}`
          ].join("\n");

          vscode.window.showErrorMessage(
            errorMessage,
            { modal: true },
          );
          return false;
      }

      const pathToSnippet = vscode.Uri.file(context.asAbsolutePath("resources/config_snippets/config" + lang));
      const snippetContents = await vscode.workspace.fs.readFile(pathToSnippet);

      const workspaceEdit = new vscode.WorkspaceEdit();
      workspaceEdit.createFile(pickedFile, {
        contents: snippetContents,
        overwrite: true
      });

      await vscode.workspace.applyEdit(workspaceEdit);

      await vscode.workspace
        .openTextDocument(pickedFile)
        .then(config => {
          vscode.window.showTextDocument(config);
        });

      vscode.window.showInformationMessage("Don't forget to update your launch.json config",
        { modal: true, detail: "Add a path or a (*@AskOnStart@*) command to your config file into the 'configPath' field." }
      );

      return true;
    }

    return false;
  });
}
