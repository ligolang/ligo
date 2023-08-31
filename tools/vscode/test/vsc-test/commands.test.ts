import { suite } from 'mocha'

import { initializeTests, testDriver } from '../commands-common'

type CompileContract = { entrypoint?: string; output: RegExp }
type CompileStorage = { entrypoint?: string; storage: string; output: RegExp }
type CompileExpression = { expression: string; output: RegExp }
type DryRun = { entrypoint?: string; parameter: string; storage: string; output: RegExp }
type EvaluateFunction = { function?: string; argument: string, output: RegExp }
type EvaluateValue = { value: string, output: RegExp }

type Expectation = {
  testFile: string
  compileContract?: CompileContract
  compileStorage?: CompileStorage
  compileExpression?: CompileExpression
  dryRun?: DryRun
  evaluateFunction?: EvaluateFunction
  evaluateValue?: EvaluateValue
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

function runTestsForFile(expectation: Expectation): void {
  initializeTests(expectation.testFile)
  suite(`Run LIGO Options commands for ${expectation.testFile}`, () => {
    if (expectation.compileContract) compileContract(expectation.compileContract)
    if (expectation.compileStorage) compileStorage(expectation.compileStorage)
    if (expectation.compileExpression) compileExpression(expectation.compileExpression)
    if (expectation.dryRun) dryRun(expectation.dryRun)
    if (expectation.evaluateFunction) evaluateFunction(expectation.evaluateFunction)
    if (expectation.evaluateValue) evaluateValue(expectation.evaluateValue)
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
      argument: 'unit',
      output: /"\[lambda of type: \(lambda unit \(pair \(list operation\) unit\)\) \]"/,
    },
    evaluateValue: {
      value: 'main',
      output: /"\[lambda of type: \(lambda unit \(lambda unit \(pair \(list operation\) unit\)\)\) \]"/,
    },
  }))
})
