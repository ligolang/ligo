import * as cp from 'child_process'
import * as glob from 'glob'
import * as path from 'path'
import { parse } from 'ts-command-line-args'
import { resolveCliPathFromVSCodeExecutablePath, runTests } from '@vscode/test-electron'

type TestSettings = {
  extensionDevelopmentPath?: string
  extensionTestsPath?: string
  vscodeExecutablePath?: string
}

const args: TestSettings = parse<TestSettings>({
  extensionDevelopmentPath: { type: String, optional: true },
  extensionTestsPath: { type: String, optional: true },
  vscodeExecutablePath: { type: String, optional: true },
});

(async () => {
  try {
    // The folder containing the Extension Manifest package.json
    // Passed to `--extensionDevelopmentPath`
    const extensionDevelopmentPath = args.extensionDevelopmentPath ?? path.resolve(__dirname, '../')

    // The path to the extension test script
    // Passed to --extensionTestsPath
    const extensionTestsPath = args.extensionTestsPath ?? path.resolve(__dirname, './vsc-test/index.js')

    // The path to VS Code itself, passed to --vscodeExecutablePath
    const vscodeExecutablePath = args.vscodeExecutablePath ?? ''

    glob.glob(path.join(extensionDevelopmentPath, 'ligo-vscode-*.vsix'), (err, files) => {
      if (err) {
        throw err
      }

      cp.spawnSync(
        resolveCliPathFromVSCodeExecutablePath(vscodeExecutablePath),
        ['--install-extension', files[0]],
        {
          encoding: 'utf-8',
          stdio: 'inherit',
        },
      )
    })

    await runTests({ extensionDevelopmentPath, extensionTestsPath, vscodeExecutablePath })
  } catch (err) {
    console.error(`Failed to run tests with ${err}`)
    process.exit(1)
  }
})()
