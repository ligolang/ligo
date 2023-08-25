// tslint:disable: no-console
import * as assert from 'assert'
import * as path from 'path'
import * as vscode from 'vscode'

import { contractsDir, delay, installLigoLibrary } from '../common'

const check = (shouldCompile: boolean) => (result) => {
  if (shouldCompile) {
    assert(result !== undefined, "should compile, but didn't");
  }
  else {
    assert(result === undefined, "shouldn't compile, but did");
  }
}

async function acceptQuickPick() {
  // Wait for `showQuickPick` to appear
  await delay(1000)
  // Accept quickPick select
  vscode.commands.executeCommand('workbench.action.acceptSelectedQuickOpenItem')
}

function compileFileWithLibary(file: string, shouldCompile: boolean) {
  test(`Contract ${file} should${shouldCompile ? "" : " not"} compile`, async () => {
    const uri = vscode.Uri.file(file);
    const doc = await vscode.workspace.openTextDocument(uri);
    await vscode.window.showTextDocument(doc);
    let result = vscode.commands.executeCommand('ligo.compileContract', uri);

    return Promise.all<any>([
      acceptQuickPick(),
      result.then(check(shouldCompile)),
    ]);
  }).timeout(50000)
}

suite('LIGO: External libs ', () => {
  const testFolder = path.join(contractsDir, 'bugs', 'LIGO-821', '1');
  installLigoLibrary(path.join(testFolder, '2'))
  installLigoLibrary(testFolder)
  compileFileWithLibary(path.join(testFolder, '2', 'lib1.jsligo'), false)
  compileFileWithLibary(path.join(testFolder, '2', 'lib2.mligo'), true)
  compileFileWithLibary(path.join(testFolder, '3', '4', 'lib1.jsligo'), true)
  compileFileWithLibary(path.join(testFolder, '3', '4', 'lib2.mligo'), false)
});
