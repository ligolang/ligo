import { assert, expect } from 'chai'
import { suite, test } from 'mocha'
import * as ncp from 'copy-paste'
import * as path from 'path'
import * as tester from 'vscode-extension-tester'

const contractsDir: string = process.env.CONTRACTS_DIR || path.join(__dirname, '..', '..', '..', '..', 'test', 'contracts')

const defaultTimeout = 90000
const delayBetweenCopies = 5000

type CompileContract = { entrypoint?: string; output: RegExp }
type CompileStorage = { entrypoint?: string; storage: string; output: RegExp }
type CompileExpression = { expression: string; output: RegExp }
type DryRun = { entrypoint?: string; parameter: string; storage: string; output: RegExp }
type EvaluateFunction = { function?: string; argument: string, output: RegExp }
type EvaluateValue = { value: string, output: RegExp }
type Deploy = { entrypoint?: string, storage: string, network: string, output: RegExp }
type GenerateDeployScript = { entrypoint?: string, storage: string, network: string, output: RegExp }

type Expectation = {
  testFile: string
  compileContract?: CompileContract
  compileStorage?: CompileStorage
  compileExpression?: CompileExpression
  dryRun?: DryRun
  evaluateFunction?: EvaluateFunction
  evaluateValue?: EvaluateValue
  deploy?: Deploy
  generateDeployScript?: GenerateDeployScript
}

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
  const input = await tester.InputBox.create();
  await input.setText(file);
  await input.confirm();
}

function testDriver(
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

function compileContract(expected: CompileContract): void {
  testDriver(
    'Compile contract',
    'LIGO Options',
    'Should compile the contract',
    expected.output,
    expected.entrypoint,
  )
}

function compileStorage(expected: CompileStorage): void {
  testDriver(
    'Compile storage',
    'LIGO Options',
    'Should compile the storage',
    expected.output,
    expected.entrypoint,
    expected.storage,
  )
}

function compileExpression(expected: CompileExpression): void {
  testDriver(
    'Compile expression',
    'LIGO Options',
    'Should compile main',
    expected.output,
    expected.expression,
  )
}

function dryRun(expected: DryRun): void {
  testDriver(
    'Dry run',
    'LIGO Options',
    'Should dry run the contract',
    expected.output,
    expected.entrypoint,
    expected.parameter,
    expected.storage,
  )
}

function evaluateFunction(expected: EvaluateFunction): void {
  testDriver(
    'Evaluate function',
    'LIGO Options',
    'Should evaluate main',
    expected.output,
    expected.function,
    expected.argument,
  )
}

function evaluateValue(expected: EvaluateValue): void {
  testDriver(
    'Evaluate value',
    'LIGO Options',
    'Should evaluate main',
    expected.output,
    expected.value,
  )
}

function deploy(expected: Deploy): void {
  testDriver(
    'Deploy contract',
    'Deploy LIGO',
    'Should deploy the contract to ghostnet',
    expected.output,
    expected.entrypoint,
    expected.storage,
    expected.network,
  )
}

function generateDeployScript(expected: GenerateDeployScript): void {
  testDriver(
    'Generate deploy script',
    'Deploy LIGO',
    'Should generate a deploy script for the contract',
    expected.output,
    expected.entrypoint,
    expected.storage,
    expected.network,
  )
}

function runTestsForFile(expectation: Expectation): void {
  const file = path.normalize(path.join(contractsDir, expectation.testFile))
  test('Open file and other stuff for tests', async () => {
    await openEditor(file)
    // Wait for a minute so everything loads...
    await delay(60000)
  }).timeout(defaultTimeout)
  suite(`Run LIGO Options commands for ${expectation.testFile}`, () => {
    if (expectation.compileContract) compileContract(expectation.compileContract)
    if (expectation.compileStorage) compileStorage(expectation.compileStorage)
    if (expectation.compileExpression) compileExpression(expectation.compileExpression)
    if (expectation.dryRun) dryRun(expectation.dryRun)
    if (expectation.evaluateFunction) evaluateFunction(expectation.evaluateFunction)
    if (expectation.evaluateValue) evaluateValue(expectation.evaluateValue)
  })
  suite(`Run Deploy LIGO commands for ${expectation.testFile}`, () => {
    if (expectation.deploy) deploy(expectation.deploy)
    if (expectation.generateDeployScript) generateDeployScript(expectation.generateDeployScript)
  })
}

suite('LIGO: Commands work', () => {
  runTestsForFile(({
    testFile: 'simple.mligo',
    compileContract: {
      output: /{ parameter unit ; storage unit ; code { CAR ; NIL operation ; PAIR } }/,
    },
    compileStorage: {
      storage: 'unit',
      output: /Unit/,
    },
    compileExpression: {
      expression: 'main',
      output: /{ CAR ; NIL operation ; PAIR }/,
    },
    dryRun: {
      parameter: 'unit',
      storage: 'unit',
      output: /\( LIST_EMPTY\(\) , unit \)/,
    },
    evaluateFunction: {
      argument: 'unit, unit',
      output: /\( LIST_EMPTY\(\) , unit \)/,
    },
    evaluateValue: {
      value: 'main',
      output: /"\[lambda of type: \(lambda \(pair unit unit\) \(pair \(list operation\) unit\)\) \]"/,
    },
    deploy: {
      storage: 'unit',
      network: 'ghostnet',
      output: /The contract was successfully deployed on the ghostnet test network\nView your contract here: https:\/\/better-call\.dev\/ghostnet\/[a-zA-Z0-9]{36}\nThe address of your new contract is: [a-zA-Z0-9]{36}\nThe initial storage of your contract is: { *"prim": *"Unit" *}/,
    },
    generateDeployScript: {
      storage: 'unit',
      network: 'ghostnet',
      output: /Generated deploy script for 'simple' contract:\noctez-client originate contract simple transferring 0 from baker running { parameter unit ; storage unit ; code { CAR ; NIL operation ; PAIR } }\n\n --init Unit\n\n --burn-cap 0.07375/,
    },
  }))
  runTestsForFile(({
    testFile: 'simple.mligo',
    deploy: {
      storage: 'unit',
      network: 'nairobinet',
      output: /The contract was successfully deployed on the nairobinet test network\nView your contract here: https:\/\/better-call\.dev\/nairobinet\/[a-zA-Z0-9]{36}\nThe address of your new contract is: [a-zA-Z0-9]{36}\nThe initial storage of your contract is: { *"prim": *"Unit" *}/,
    },
    generateDeployScript: {
      storage: 'unit',
      network: 'nairobinet',
      output: /Generated deploy script for 'simple' contract:\noctez-client originate contract simple transferring 0 from baker running { parameter unit ; storage unit ; code { CAR ; NIL operation ; PAIR } }\n\n --init Unit\n\n --burn-cap 0.07375/,
    },
  }))
})
