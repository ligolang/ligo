// tslint:disable: no-console
import * as assert from 'assert'
import * as path from 'path'
import * as vscode from 'vscode'

import { contractsDir, installLigoLibrary } from '../common'

const check = (shouldCompile: boolean) => (result) => {
  if (shouldCompile) {
    assert(result !== undefined);
  }
  else {
    assert(result === undefined);
  }
}

function delay(ms: number) {
  return new Promise( resolve => setTimeout(resolve, ms) );
}

async function acceptEntrypoint() {
  // Wait for `showQuickPick` to appear
  await delay(1000);
  // Accept quickPick select
  vscode.commands.executeCommand('workbench.action.acceptSelectedQuickOpenItem');
}

function compileFileWithLibary(file: string, shouldCompile: boolean) {
  test(`Contract ${file} should ${shouldCompile ? "" : "not"} compile`, async () => {
    const uri = await vscode.Uri.file(file);
    const doc = await vscode.workspace.openTextDocument(uri);
    await vscode.window.showTextDocument(doc);
    let result = vscode.commands.executeCommand('ligo.compileContract', uri);

    return Promise.all<any>([
      acceptEntrypoint(),
			result.then(check(shouldCompile))
		]);
  }).timeout(50000)
}

suite('LIGO: External libs ', () => {
  const testFolder = path.join(contractsDir, 'bugs', 'LIGO-821', '1');
  installLigoLibrary(path.join(testFolder, '2'))
  installLigoLibrary(testFolder)
  compileFileWithLibary(path.join(testFolder, '2', 'lib1.jsligo'), false)
  compileFileWithLibary(path.join(testFolder, '2', 'lib2.jsligo'), true)
  compileFileWithLibary(path.join(testFolder, '3', '4', 'lib1.jsligo'), true)
  compileFileWithLibary(path.join(testFolder, '3', '4', 'lib2.jsligo'), false)
});
