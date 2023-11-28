import { assert, expect } from 'chai'
import { test } from 'mocha'
import * as ncp from 'copy-paste'
import * as tester from 'vscode-extension-tester'
import * as path from 'path'

const contractsDir: string = process.env.CONTRACTS_DIR || path.join(__dirname, '..', '..', '..', 'test', 'contracts')

const defaultTimeout = 90000
const delayBetweenCopies = 5000

async function delay(delay: number): Promise<void> {
  return await new Promise((res) => setTimeout(res, delay))
}

// Deploying takes time, so we try to copy until we find something (or the test
// timeouts).
async function copyUntilHasOutput(workbench: tester.Workbench): Promise<string> {
  // We should be able to use getText on the output channel but it doesn't work.
  // Seems like the underlying command that vscode-extension-tester uses doesn't
  // work, so we replicate it here but using 'editor.action.selectAll'.
  await workbench.executeCommand('editor.action.selectAll')
  await workbench.executeCommand('editor.action.clipboardCopyAction')
  const output = ncp.paste().trim() // FIXME: overwrites the clipboard...
  if (output === '') {
    await delay(delayBetweenCopies)
    return await copyUntilHasOutput(workbench)
  } else {
    return output
  }
}

async function findButton(statusBarButtonName: string): Promise<tester.WebElement> {
  const statusBar = new tester.StatusBar()
  const ligoOptionsButtons = await statusBar.getItems()
  // We should use getItem, but it doesn't work... so we manually look for the
  // button in the available buttons.
  //const ligoOptionsButton = await statusBar.getItem(statusBarButtonName)
  for (const button of ligoOptionsButtons) {
    const title = await button.getText()
    if (title === statusBarButtonName) {
      return button
    }
  }

  return assert.fail(`Could not find a button with name ${statusBarButtonName}`)
}

// See https://github.com/redhat-developer/vscode-extension-tester/issues/506#issuecomment-1271156702
// This workaround is needed because openResource doesn't work inside docker container
// which is used to run tests in CI
async function openEditor(file: string): Promise<void> {
  const workbench = new tester.Workbench()
  await workbench.executeCommand('workbench.action.quickOpen')
  const input = await tester.InputBox.create()
  await input.setText(file)
  await input.confirm()
}

export function initializeTests(testFile: string) {
  const file = path.normalize(path.join(contractsDir, testFile))
  test('Open file and other stuff for tests', async () => {
    await openEditor(file)
    // Wait for a minute so everything loads...
    await delay(60000)
  }).timeout(defaultTimeout)
}

export function testDriver(
  buttonName: string,
  statusBarButtonName: string,
  message: string,
  expected: RegExp,
  ...sequence: (string | undefined)[]
): void {
  test(`${buttonName}: ${message}`, async () => {
    const ligoButton = await findButton(statusBarButtonName)
    await ligoButton.click()
    const optionsMenu = await tester.InputBox.create()
    await optionsMenu.selectQuickPick(buttonName)

    for (const input of sequence) {
      const inputBox = await tester.InputBox.create()
      if (input) {
        await inputBox.sendKeys(input)
      }
      await inputBox.confirm()

      // Just waiting for some validation stuff
      await delay(2000)
    }

    const bottomBar = new tester.BottomBarPanel()
    await bottomBar.wait(30000)
    await bottomBar.toggle(true)
    const outputChannel = await bottomBar.openOutputView()
    await outputChannel.selectChannel('LIGO Compiler')
    await outputChannel.click()
    const workbench = new tester.Workbench()
    const output = await copyUntilHasOutput(workbench)
    await outputChannel.clearText()
    await bottomBar.toggle(false)
    expect(output).to.match(expected)
  }).timeout(defaultTimeout)
}
