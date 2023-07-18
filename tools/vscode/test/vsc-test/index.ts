/* eslint-disable import/prefer-default-export */
import * as glob from 'glob'
import * as Mocha from 'mocha'
import * as path from 'path'
import * as vscode from 'vscode'

export async function activateExtension() {
  const extension = vscode.extensions.getExtension<any>('ligolang-publish.ligo-vscode')!;
  const api = await extension.activate();
  // Wait until its ready to use.
  await api.ready;
  return api;
}

export function run(): Promise<void> {
  // Create the mocha test
  const mocha = new Mocha({
    ui: 'tdd',
    color: true,
  })

  const testsRoot = path.resolve(__dirname, '..')

  return new Promise((c, e) => {
    glob('**/**.test.js', { cwd: testsRoot }, (err: any, files: string[]) => {
      if (err) {
        e(err)
        return
      }

      // Add files to the test suite
      files.forEach((f) => {
        // TODO: eventually rewrite every test with vscode-extension-tester and
        // change package.json to only use extest. This file can be deleted
        // after that.
        const basename = path.basename(f, '.js')
        if (basename !== 'commands.test' && basename !== 'commands-deploy.test' && basename !== 'commands-common') {
          mocha.addFile(path.resolve(testsRoot, f))
        }
      })

      try {
        // Run the mocha test
        mocha.run((failures) => {
          if (failures > 0) {
            e(new Error(`${failures} tests failed.`))
          }

          c()
        })
      } catch (err2) {
        e(err2)
      }
    })
  })
}
