import notification from '~/base-components/notification'
import redux from '~/base-components/redux'

import { ProjectManager, BaseProjectManager } from '~/base-components/workspace'
import { modelSessionManager } from '~/base-components/code-editor'

import { networkManager } from '~/ligo-components/eth-network'
import compilerManager, { CompilerManager } from '~/ligo-components/eth-compiler'
import queue from '~/ligo-components/eth-queue'

import debounce from 'lodash/debounce'
import moment from 'moment'

import ProjectSettings from './ProjectSettings'

BaseProjectManager.ProjectSettings = ProjectSettings

function makeProjectManager(Base) {
  return class ExtendedProjectManager extends Base {
    constructor(project, projectRoot) {
      super(project, projectRoot)
      this.deployButton = null
      this.onFileChanged = debounce(this.onFileChanged, 1500).bind(this)
    }

    get settingsFilePath() {
      return this.pathForProjectFile('config.json')
    }

    onEditorReady(editor, editorComponent) {
      modelSessionManager.decorationMap = {}
      editor.addCommand(
        monaco.KeyMod.CtrlCmd | monaco.KeyCode.KEY_S,
        () => {
          editorComponent.props.onCommand('save')
          this.lint()
        }
      )
      editor.onDidChangeModel(() => setTimeout(() => this.lint(), 100))
      setTimeout(() => this.lint(), 100)
      // reset the editor model sessions
    }

    onFileChanged() {
      this.lint()
    }

    async readPackageJson() {
      // const packageJson = await this.readFile(this.pathForProjectFile('package.json'))
      return {} //JSON.parse(packageJson)
    }

    async executeInTerminal(cmd) {
      this.toggleTerminal(true)
      return await compilerManager.execute(cmd)
    }

    lint() {
      return
    }

    async compile(sourceFile, finalCall) {
      if (CompilerManager.button.state.building) {
        notification.error('Build Failed', 'Another build task is running now.')
        return false
      }

      const settings = await this.checkSettings()

      await this.project.saveAll()
      this.toggleTerminal(true)

      let result
      try {
        result = await compilerManager.build(settings, this, sourceFile)
      } catch {
        finalCall && finalCall()
        return false
      }
      if (result?.decorations) {
        modelSessionManager.updateDecorations(result.decorations)
      }
      if (result?.errors) {
        finalCall && finalCall()
        return false
      }
       
      finalCall && finalCall()
      return true
    }

    async deploy(contractFileNode) {
      if (!networkManager.sdk) {
        notification.error('Cannot Deploy', 'No connected network. Please start a local network or switch to a remote network.')
        return true
      }

      let contracts
      if (contractFileNode) {
        contractFileNode.pathInProject = this.pathInProject(contractFileNode.path)
        contracts = [contractFileNode]
      } else {
        try {
          contracts = await this.getBuiltContracts()
        } catch {
          notification.error('Cannot Deploy', `Cannot locate the built folder. Please make sure you have built the project successfully.`)
          return
        }
      }

      if (!contracts.length) {
        notification.error('Cannot Deploy', `No built contracts found. Please make sure you have built the project successfully.`)
        return
      }

      this.deployButton.getDeploymentParameters({
        contractFileNode: contractFileNode || await this.getDefaultContractFileNode(),
        contracts,
      },
        (contractObj, allParameters) => this.pushDeployment(contractObj, allParameters),
        (contractObj, allParameters) => this.estimate(contractObj, allParameters)
      )
    }

    async getDefaultContractFileNode() {
      const settings = await this.checkSettings()
      if (!settings?.deploy) {
        return
      }
      const filePath = this.pathForProjectFile(settings.deploy)
      const pathInProject = this.pathInProject(filePath)
      return { path: filePath, pathInProject }
    }

    async getBuiltContracts() {
      const settings = await this.checkSettings()
      const builtFolder = this.pathForProjectFile(
        settings.framework === 'hardhat' ? 'artifacts/contracts' : 'build/contracts'
      )
      let stopCriteria
      if (settings.framework === 'hardhat') {
        stopCriteria = child => child.type === 'file' && child.name.endsWith('.json') && !child.name.endsWith('.dbg.json')
      } else {
        stopCriteria = child => child.type === 'file' && child.name.endsWith('.json')
      }
      const files = await this.readDirectoryRecursively(builtFolder, stopCriteria)
      return files.map(f => ({ ...f, pathInProject: this.pathInProject(f.path) }))
    }

    async readProjectAbis() {
      const contracts = await this.getBuiltContracts()
      const abis = await Promise.all(contracts
        .map(contract => this.readFile(contract.path)
          .then(content => ({
            contractPath: contract.path,
            pathInProject: this.pathInProject(contract.path),
            content: JSON.parse(content)
          }))
          .catch(() => null)
        )
      )
      return abis
        .filter(Boolean)
        .map(({ contractPath, pathInProject, content }) => {
          const name = content.contractName || this.path.parse(contractPath).name
          return { contractPath, pathInProject, name, abi: content?.abi, content }
        })
    }

    checkSdkAndSigner(allParameters) {
      if (!networkManager.sdk) {
        notification.error('No Network', 'No connected network. Please start a local network or switch to a remote network.')
        return true
      }

      if (!allParameters.signer) {
        notification.error('Deployment Error', 'No signer specified. Please select one to sign the deployment transaction.')
        return true
      }
    }

    validateDeployment(contractObj) {
      let bytecode = contractObj.bytecode || contractObj.evm?.bytecode?.object
      let deployedBytecode = contractObj.deployedBytecode || contractObj.evm?.deployedBytecode?.object

      if (!deployedBytecode) {
        notification.error('Deployment Error', `Invalid <b>deployedBytecode</b> and <b>evm.deployedBytecode.object</b> fields in the built contract JSON. Please make sure you selected a correct built contract JSON file.`)
        return
      }
      if (!deployedBytecode) {
        notification.error('Deployment Error', `Invalid <b>bytecode</b> and <b>evm.bytecode.object</b> fields in the built contract JSON. Please make sure you selected a correct built contract JSON file.`)
        return
      }
      if (!bytecode.startsWith('0x')) {
        bytecode = '0x' + bytecode
      }
      if (!deployedBytecode.startsWith('0x')) {
        deployedBytecode = '0x' + deployedBytecode
      }
      return {
        abi: contractObj.abi,
        bytecode,
        deployedBytecode
      }
    }

    async estimate(contractObj, allParameters) {
      if (this.checkSdkAndSigner(allParameters)) {
        return
      }
      const deploy = this.validateDeployment(contractObj)
      if (!deploy) {
        return
      }

      const { amount, parameters } = allParameters

      this.deployButton.setState({ pending: 'Estimating...' })

      let result
      try {
        const tx = await networkManager.sdk.getDeployTransaction({
          abi: deploy.abi,
          bytecode: deploy.bytecode,
          options: deploy.options,
          parameters: parameters.array,
          amount,
        }, {
          from: allParameters.signer,
        })
        result = await networkManager.sdk.estimate(tx)
      } catch (e) {
        console.warn(e)
        notification.error('Estimate Failed', e.reason || e.message)
        this.deployButton.setState({ pending: false })
        return
      }

      this.deployButton.setState({ pending: false })

      return result
    }

    async pushDeployment(contractObj, allParameters) {
      if (this.checkSdkAndSigner(allParameters)) {
        return
      }
      const deploy = this.validateDeployment(contractObj)
      if (!deploy) {
        return
      }

      this.deployButton.setState({ pending: 'Deploying...', result: '' })

      const networkId = networkManager.sdk.networkId
      const { contractName, amount, parameters, ...override } = allParameters
      const codeHash = networkManager.sdk.utils.sign.sha3(deploy.deployedBytecode)

      let result
      try {
        const tx = await networkManager.sdk.getDeployTransaction({
          abi: deploy.abi,
          bytecode: deploy.bytecode,
          options: deploy.options,
          parameters: parameters.array,
          amount,
        }, {
          from: allParameters.signer,
          ...override
        })

        result = await new Promise((resolve, reject) => {
          queue.add(
            () => networkManager.sdk.sendTransaction(tx),
            {
              title: 'Deploy a Contract',
              name: 'Deploy',
              contractName,
              signer: allParameters.signer,
              abi: deploy.abi,
              value: networkManager.sdk.utils.unit.toValue(amount || '0'),
              params: parameters.obj,
              ...override,
              modalWhenExecuted: true,
            },
            {
              pushing: () => this.deployButton.closeModal(),
              executed: ({ tx, receipt, abi }) => {
                resolve({
                  network: networkId,
                  codeHash,
                  ...parameters,
                  tx,
                  receipt,
                  abi,
                })
                return true
              },
              'failed-timeout': reject,
              failed: reject,
            }
          ).catch(reject)
        })
      } catch (e) {
        console.warn(e)
        notification.error('Deploy Failed', e.reason || e.message)
        this.deployButton.setState({ pending: false })
        return
      }

      this.deployButton.setState({ pending: false })
      notification.success('Deploy Successful')

      redux.dispatch('ABI_ADD', {
        ...deploy.options,
        name: contractName,
        codeHash: result.codeHash,
        abi: JSON.stringify(deploy.abi),
      })

      const deployResultPath = this.path.join(this.projectRoot, 'deploys', `${result.network}_${moment().format('YYYYMMDD_HHmmss')}.json`)
      await this.ensureFile(deployResultPath)
      await this.saveFile(deployResultPath, JSON.stringify(result, null, 2))
    }
  }
}

export default {
  Local: makeProjectManager(ProjectManager.Local),
}
