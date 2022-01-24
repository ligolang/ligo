import * as assert from 'assert'
import * as fs from 'fs'
import * as path from 'path'
import * as vscode from 'vscode'

import activate, { contractsDir } from '../common'

const oldContractsDir: string = path.join(contractsDir, 'rename-directory')
const newContractsDir: string = path.join(contractsDir, 'renamed-directory')

suite('LIGO: Rename directory', () => {
  test('Renaming does not make the extension stop working', async () => {
    try {
      const uri = await vscode.Uri.file(path.join(oldContractsDir, 'LIGO-320.mligo'))
      const position = new vscode.Position(1, 5)

      await activate(uri)

      const oldDef = await vscode.commands.executeCommand('vscode.executeDefinitionProvider', uri, position)
      fs.renameSync(oldContractsDir, newContractsDir)
      const newDef = await vscode.commands.executeCommand('vscode.executeDefinitionProvider', uri, position)

      assert.ok(oldDef)
      assert.ok(newDef)
      assert.deepStrictEqual(oldDef, newDef)
    } finally {
      const oldDirExists = fs.existsSync(oldContractsDir)
      const newDirExists = fs.existsSync(newContractsDir)

      if (oldDirExists) {
        if (newDirExists) {
          // Something went wrong: there is a duplicate. This shouldn't happen,
          // but let's just remove one of them...
          fs.rmdirSync(newContractsDir)
        } else {
          // Something went wrong: test failed before/during renaming
        }
      } else if (newDirExists) {
        // OK: New directory will be leftover once the test has finished,
        // revert it
        fs.renameSync(newContractsDir, oldContractsDir)
      } else {
        // Something went wrong: both directories ceased to exist somehow
        assert.fail("Directory for test doesn't exist")
      }
    }
  }).timeout(5000) // 2000 is not enough
})
