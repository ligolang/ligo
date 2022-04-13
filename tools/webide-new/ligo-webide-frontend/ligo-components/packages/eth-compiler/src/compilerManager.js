import { DockerImageChannel } from '@obsidians/docker'
import notification from '@obsidians/notification'
import fileOps from '@obsidians/file-ops'
import stripAnsi from 'strip-ansi'
import { modelSessionManager } from '@obsidians/code-editor'

import SolcjsCompiler from './SolcjsCompiler'
import soljsonReleases from './soljsonReleases.json'

class SolcjsChannel extends DockerImageChannel {
  installed() {
    return true
  }

  versions() {
    const versions = Object.entries(soljsonReleases).map(([Tag, Name]) => ({ Tag, Name }))
    const event = new CustomEvent('versions', { detail: versions })
    this.eventTarget.dispatchEvent(event)
    return versions
  }
}

export class CompilerManager {
  static button = null
  static terminal = null
  static truffleTerminal = null

  constructor() {
    this.truffle = new DockerImageChannel(process.env.DOCKER_IMAGE_COMPILER)
    this.solc = new SolcjsChannel()
    this.notification = null
    this.solcjsCompiler = new SolcjsCompiler()
  }

  get projectRoot() {
    if (!CompilerManager.terminal) {
      throw new Error('CompilerTerminal is not instantiated.')
    }
    return CompilerManager.terminal.props.cwd
  }

  focus() {
    if (CompilerManager.terminal) {
      CompilerManager.terminal.focus()
    }
  }

  async execute(cmd) {
    CompilerManager.switchCompilerConsole('terminal')
    return await CompilerManager.terminal?.exec(cmd)
  }

  async cacheSolcBin(url, version) {
    const cacheStorage = await window.caches.open('solcjs')
    try {
      if (await cacheStorage.match(url)) {
        return
      }
    } catch {
      await cacheStorage.delete(url)
    }

    this.notification = notification.info(`Downloading Solc Bin`, `Downloading <b>${version}</b>...`, 0)
    const request = new Request(url, { mode: 'no-cors' })
    const response = await fetch(request)
    await cacheStorage.put(url, response)
    this.notification.dismiss()
  }

  async buildBySolcjs(projectManager) {
    if (!await projectManager.isMainValid) {
      notification.error('No Main File', `Please specify the main file in project settings.`)
      throw new Error('No Main File.')
    }

    const solcVersion = projectManager.projectSettings.get('compilers.solc')
    const solcFileName = soljsonReleases[solcVersion]

    // TODO: use the production proxy temporally
    const solcUrl = `https://eth.ide.black/solc/${solcFileName}`

    const evmVersion = projectManager.projectSettings.get('compilers.evmVersion')
    const optimizer = projectManager.projectSettings.get('compilers.optimizer')

    CompilerManager.button.setState({ building: true })
    try {
      await this.cacheSolcBin(solcUrl, solcFileName)
    } catch (e) {
      console.error(e)
      CompilerManager.button.setState({ building: false })
      throw e
    }

    CompilerManager.terminal.writeCmdToTerminal(`solcjs --bin ${projectManager.projectSettings.get('main')}`, `[${solcFileName}]`)
    this.notification = notification.info(`Building Project`, `Building...`, 0)

    let output
    try {
      output = await this.solcjsCompiler.compile(solcUrl, projectManager)
    } catch (e) {
      this.notification.dismiss()
      notification.error('Build Failed', e.message)
      CompilerManager.button.setState({ building: false })
      throw e
    }

    if (!output) {
      this.notification.dismiss()
      notification.error('Build Failed', ``)
      CompilerManager.button.setState({ building: false })
      throw new Error('Build Failed.')
    }

    if (output.contracts) {
      for (const file in output.contracts) {
        for (const contractName in output.contracts[file]) {
          const json = output.contracts[file][contractName]
          const contractJsonPath = projectManager.pathForProjectFile(`build/contracts/${contractName}.json`)
          const contractJson = JSON.stringify(json, null, 2)
          await projectManager.saveFile(contractJsonPath, contractJson)
        }
      }
      projectManager.refreshDirectory()
      projectManager.refreshDirectory(projectManager.pathForProjectFile('build/contracts'))
    }
    const errorDecorations = []
    let hasError = false

    output.errors?.forEach(error => {
      let color
      if (error.severity === 'error') {
        hasError = true
        color = '--color-danger'
        errorDecorations.push(this.parseSolcJSBuild(error))
      } else if (error.severity === 'warning') {
        color = '--color-warning'
      }
      CompilerManager.terminal.writeToTerminal(error.type, color)
      CompilerManager.terminal.writeToTerminal(`${error.formattedMessage.replace(error.type, '').replace(/\n/g, '\n\r')}`)
    })

    this.notification.dismiss()
    CompilerManager.button.setState({ building: false })
    if (hasError) {
      notification.error('Build Failed', `Code has errors.`)
      modelSessionManager.updateDecorations(errorDecorations)
    } else {
      notification.success('Build Successful', `The smart contract is built.`)
      modelSessionManager.clearDecoration('compiler')
    }
  }

  async build(settings, projectManager, sourceFile) {
    if (projectManager.remote) {
      return await this.buildBySolcjs(projectManager)
    }

    const { framework, compilers = {} } = settings

    const projectRoot = this.projectRoot
    CompilerManager.button.setState({ building: 'checking' })

    if (framework.endsWith('-docker')) {
      if (!compilers || !compilers[process.env.COMPILER_VERSION_KEY]) {
        notification.error(`No ${process.env.COMPILER_NAME} Version`, `Please select a version for ${process.env.COMPILER_NAME} in project settings.`)
        CompilerManager.button.setState({ building: false })
        throw new Error(`No ${process.env.COMPILER_NAME} version.`)
      }
      const allVersions = await this.truffle.versions()
      if (!allVersions.find(v => v.Tag === compilers[process.env.COMPILER_VERSION_KEY])) {
        notification.error(`${process.env.COMPILER_NAME} ${compilers[process.env.COMPILER_VERSION_KEY]} not Installed`, `Please install the version in <b>${process.env.COMPILER_NAME} Manager</b> or select another version in project settings.`)
        CompilerManager.button.setState({ building: false })
        throw new Error(`${process.env.COMPILER_NAME} version not installed`)
      }
    }

    // if (!compilers.solc) {
    //   notification.error('No Solc Version', `Please select a version for solc in project settings.`)
    //   throw new Error('No solc version.')
    // }

    const allSolcVersions = await this.solc.versions()
    if (compilers.solc && compilers.solc !== 'default' && !allSolcVersions.find(v => v.Tag === compilers.solc)) {
      notification.error(`Solc ${compilers.solc} not Installed`, `Please install the version in <b>Solc Manager</b> or select another version in project settings.`)
      CompilerManager.button.setState({ building: false })
      throw new Error('Solc version not installed')
    }

    CompilerManager.button.setState({ building: true })
    CompilerManager.switchCompilerConsole('terminal')
    if (!sourceFile) {
      this.notification = notification.info(`Building Project`, `Building...`, 0)
    } else {
      this.notification = notification.info(`Building Contract File`, `Building <b>${sourceFile}</b>...`, 0)
    }

    const cmd = this.generateBuildCmd({ projectRoot, settings, sourceFile })
    const result = await CompilerManager.terminal.exec(cmd)

    CompilerManager.button.setState({ building: false })
    this.notification.dismiss()
    await CompilerManager.terminal.execAsChildProcess(`docker rm $(docker ps --filter status=exited --filter ancestor=ethereum/solc:${compilers.solc} -q)`)

    if (result.code === 130) {
      if (result.code === 130) {
        notification.error('Build Terminated')
        return { errors: [] }
      }
    }

    const { errors, decorations } = this.parseBuildLogs(stripAnsi(result.logs))
    if (result.code) {
      if (errors.length) {
        notification.error('Build Failed', errors[0])
      } else {
        notification.error('Build Failed', `Code has errors.`)
      }
      return { errors, decorations }
    }

    const buildFolder = projectManager.path.join(framework === 'hardhat' ? 'artifacts' : 'build', 'contracts')
    if (!sourceFile) {
      notification.success('Build Project Successful', `Please find the generated ABI and bytecode in the <b>${buildFolder}</b> folder.`)
    } else {
      notification.success('Build File Successful', `Please find the generated ABI and bytecode in the <b>${buildFolder}</b> folder.`)
    }
    return { decorations }
  }

  static async stop() {
    if (CompilerManager.terminal) {
      CompilerManager.terminal.execAsChildProcess(`docker stop -t 1 truffle-compile`)
      await CompilerManager.terminal.stop()
      // await CompilerManager.terminal.execAsChildProcess(`docker rm $(docker ps --filter status=exited --filter ancestor=ethereum/solc:${compilers.solc} -q)`)
    }
  }

  parseSolcJSBuild(error) {
    const { prefix: projectPrefix, userId, projectId } = modelSessionManager.projectManager
    const [prefix] = error.formattedMessage.match(/(?<=:).+(?=:)/g)
    const filePath =  error.sourceLocation.file
    const [row, column] = prefix.split(':')
    const lines = error.formattedMessage.split('\n')
    const length = lines[lines.length - 1].trim().length

    return {
      filePath: `${projectPrefix}/${userId}/${projectId}/${filePath.replace('./', '')}`,
      text: `[Solcjs Compiler]: ${error.message}`,
      row: Number(row),
      length,
      type: 'error',
      column: Number(column),
      from: 'compiler'
    }
  }

  parseBuildLogs(msg) {
    let index
    index = msg.indexOf('Compiling your contracts...')
    if (index > -1) {
      msg = msg.substr(index + 30)
    }
    const lines = msg.split('\n')

    const errors = []
    let decorations = []
    let status = ''
    let currentBlock = ''
    lines.map(line => line.trim()).forEach(line => {
      if (!line) {
        if (status === 'ERROR') {
          errors.push(currentBlock.trim())
        } else if (status === 'DECORATION') {
          decorations.push(currentBlock.trim())
        }
        status = ''
        currentBlock = ''
      } else if (line.startsWith('Error: ') || status === 'ERROR') {
        status = 'ERROR'
        currentBlock += (line + '\n')
      } else if (line.startsWith(',/')) {
        if (status === 'DECORATION') {
          decorations.push(currentBlock.trim())
        }
        status === 'DECORATION'
        currentBlock = (line.substr(1) + '\n')
      } else if (line.startsWith('/') || status === 'DECORATION') {
        status = 'DECORATION'
        currentBlock += (line + '\n')
      }
    })
    decorations = decorations.map(msg => {
      const lines = msg.split('\n')
      const [prefix, ...rest] = lines[0].split(': ')
      const [filePath, row, column] = prefix.split(':')
      const text = rest.join(': ').trim()
      let type = 'error'
      if (text.startsWith('Warning: ')) {
        type = 'warning'
      }
      if (row && column) {
        const length = lines[lines.length - 1].trim().length
        return { filePath, type, row: Number(row), column: Number(column), length, text }
      } else {
        return { filePath, type, text }
      }
    })
    return { errors, decorations }
  }

  generateBuildCmd({ projectRoot, settings, sourceFile }) {
    const { framework, npmClient, compilers } = settings
    const projectDir = fileOps.current.getDockerMountPath(projectRoot)

    if (!framework.endsWith('-docker')) {
      const npmRun = npmClient === 'yarn' ? npmClient : `${npmClient} run`
      return `${npmRun} build`
    }

    const cmd = [
      `docker run -t --rm --name truffle-compile`,
      `-v "${fileOps.current.homePath}/.config/truffle/compilers:/root/.config/truffle/compilers"`,
      `-v "${projectDir}:${projectDir}"`,
      `-w "${projectDir}"`,
      `${process.env.DOCKER_IMAGE_COMPILER}:${compilers[process.env.COMPILER_VERSION_KEY]}`,
      `${process.env.COMPILER_EXECUTABLE_NAME} compile`,
    ]
    if (compilers.solc && compilers.solc !== 'default') {
      cmd.push(`--compilers.solc.version '${compilers.solc}'`)
    }
    if (compilers.evmVersion) {
      cmd.push(`--compilers.solc.settings.evmVersion '${compilers.evmVersion}'`)
    }
    if (compilers.optimizer?.enabled) {
      cmd.push(`--compilers.solc.settings.optimizer.enabled`)
      cmd.push(`--compilers.solc.settings.optimizer.runs ${compilers.optimizer.runs}`)
    }

    if (sourceFile) {
      cmd.push(`--contracts_directory '${sourceFile}*'`)
    }

    return cmd.join(' ')
  }
}

export default new CompilerManager()
