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

  function initializationScript() {
    const ex = new Error('Failed to initialize the LIGO extension for tests after 3 minutes');
    let timer: NodeJS.Timer | undefined;
    const failed = new Promise((_, reject) => {
        timer = setTimeout(() => reject(ex), 180_000);
    });
    const promise = Promise.race([activateExtension(), failed]);
    promise.then(() => clearTimeout(timer!)).catch(() => clearTimeout(timer!));
    return promise;
  }

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
        if (path.basename(f) !== 'commands.test.js') {
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
