import { ProjectSettings } from '~/base-components/workspace'

export default class ExtendedProjectSettings extends ProjectSettings {
  static configFileName = 'config.json'

  constructor (projectManager, settingFilePath, channel) {
    super(projectManager, settingFilePath, channel)
  }

  trimSettings = (rawSettings = {}) => {
    const compilers = rawSettings.compilers || {}
    const settings = {
      main: rawSettings.main || './contracts/Contract.sol',
      deploy: rawSettings.deploy,
      framework: rawSettings.framework || `${process.env.COMPILER_VERSION_KEY}-docker`,
      npmClient: rawSettings.npmClient,
      compilers: {
        ...compilers,
        [process.env.COMPILER_VERSION_KEY]: compilers[process.env.COMPILER_VERSION_KEY] || '',
        solc: compilers.solc || '',
        evmVersion: compilers.evmVersion || 'istanbul',
        optimizer: compilers.optimizer,
      },
      linter: rawSettings.linter || 'solhint',
      editor: {
        fontFamily: rawSettings.editor?.fontFamily || 'Hack',
        fontSize: rawSettings.editor?.fontSize || '13px',
        ligatures: Boolean(rawSettings.editor?.ligatures),
      }
    }
    if (rawSettings.language) {
      settings.language = rawSettings.language
    }
    if (!settings.npmClient) {
      delete settings.npmClient
    }
    return settings
  }

  async set (key, value) {
    await super.set(key, value)
    if (key === 'compilers.solc') {
      this.projectManager.lint()
    }
  }
}
