import { suite } from 'mocha'

import { initializeTests, testDriver } from '../commands-common'

export type Deploy = { entrypoint?: string, storage: string, network: string, output: RegExp }
export type GenerateDeployScript = { entrypoint?: string, storage: string, network: string, output: RegExp }

type Expectation = {
  testFile: string
  deploy?: Deploy
  generateDeployScript?: GenerateDeployScript
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
  initializeTests(expectation.testFile)
  suite(`Run Deploy LIGO commands for ${expectation.testFile}`, () => {
    if (expectation.deploy) deploy(expectation.deploy)
    if (expectation.generateDeployScript) generateDeployScript(expectation.generateDeployScript)
  })
}

suite('LIGO: Commands work', () => {
  runTestsForFile(({
    testFile: 'simple.mligo',
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
