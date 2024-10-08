import * as vscode from 'vscode';
import { Disposable } from "vscode-languageclient";
import { isDefined } from './base';

/**
 * When starting a process, we'd like to identify it by its name and
 * configuration path.
 */
export type BinaryInfo = {
  name: string,
  path: string,
}

/** An instance of {@link BinaryInfo} for the LIGO compiler. */
export const ligoBinaryInfo = {
  name: 'ligo',
  path: 'ligoLanguageServer.ligoBinaryPath'
}

/**
 * Attempts to get the provided {@link BinaryInfo} from the workspace
 * configuration.
 */
export function getBinaryPath(info: BinaryInfo): string {
  const config = vscode.workspace.getConfiguration()
  let binaryPath = config.get<string>(info.path)
  if (!isDefined(binaryPath)) {
    // We always expect some value because a default value is registered in package.json
    throw new Error(`Unexpectedly no ${info.name} path from config`)
  }
  return binaryPath
}

/**
 * Start detecting when path to LIGO is changed in config, and initiate client
 * restart sequence.
 */
export function trackLigoPathChanges(): Disposable {
  // We do the thing via subscribing on "configuration changed" event.
  //
  // However, this way, as the user enters the new path, we will get notified
  // about all the intermediate values. To invoke restart exactly once, when
  // config is changed we will show a prompt asking for manual restart once the
  // user is ready.

  // Helps to show the prompt only once per multiple small changes.
  var changeInProgress = false;
  return vscode.workspace.onDidChangeConfiguration(e => {
    if (!e.affectsConfiguration(ligoBinaryInfo.path)) {
      return
    }

    if (changeInProgress) {
      return
    }
    changeInProgress = true

    vscode.window.showInformationMessage(
      "Restart is needed for LIGO path change to take effect.",
      "Restart",
      "Cancel"
    ).then(
      selectedValue => {
        changeInProgress = false
        switch (selectedValue) {
          case "Restart":
            vscode.commands.executeCommand('workbench.action.reloadWindow');
            return;
          case "Cancel":
          case undefined:
        }
      }
    )
  })
}
