import * as path from 'path'
import { parse } from 'ts-command-line-args'
import { runTests } from '@vscode/test-electron'

type TestSettings = {
  extensionDevelopmentPath?: string
  extensionTestsPath?: string
  vscodeExecutablePath?: string
}

const args: TestSettings = parse<TestSettings>({
  extensionDevelopmentPath: { type: String, optional: true },
  extensionTestsPath: { type: String, optional: true },
  vscodeExecutablePath: { type: String, optional: true },
}, {
  partial: true
});

(async () => {
  try {
    // The folder containing the Extension Manifest package.json
    // Passed to `--extensionDevelopmentPath`
    const extensionDevelopmentPath = args.extensionDevelopmentPath ?? path.resolve(__dirname, '../../../')

    // The path to the extension test script
    // Passed to --extensionTestsPath
    const extensionTestsPath = args.extensionTestsPath ?? path.resolve(__dirname, './vsc-test/index.js')

    // The path to VS Code itself, passed to --vscodeExecutablePath
    const vscodeExecutablePath = args.vscodeExecutablePath ?? ''

    await runTests({
      extensionDevelopmentPath,
      extensionTestsPath,
      vscodeExecutablePath,
      launchArgs: [
        '--disable-extensions',
      ],
    })
  } catch (err) {
    console.error(`Failed to run tests with ${err}`)
    process.exit(1)
  }
})()
