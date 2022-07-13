import Workspace from '~/base-components/workspace'
import fileOps from '~/base-components/file-ops'
import { useBuiltinCustomTabs, modelSessionManager, defaultModeDetector } from '~/base-components/code-editor'
import compilerManager, { CompilerTerminal } from '~/ligo-components/eth-compiler'
import platform from '~/base-components/platform'
import ProjectManager from '../ProjectManager'

import ProjectToolbar from './ProjectToolbar'
import ProjectSettingsTab from './ProjectSettingsTab'

import addSolidityLanguage from './languages/solidity'
import findIndex from 'lodash/findIndex'

useBuiltinCustomTabs(['markdown'])
modelSessionManager.registerCustomTab('settings', ProjectSettingsTab, 'Project Settings')
modelSessionManager.registerModeDetector(filePath => {
    const { prefix, userId, projectId, settingsFilePath } = modelSessionManager.projectManager
    const { base } = fileOps.pathHelper.parse(filePath)
    const settingFilePath = settingsFilePath // platform.isDesktop ? settingsFilePath : `${prefix}/${userId}/${projectId}/config.json`
    const isRoot = settingFilePath === filePath

    if (base === 'config.json' && isRoot) {
      return 'settings'
    } else if (base.endsWith('.sol')) {
      return 'solidity'
    } else if (base.endsWith('.religo')) {
      return 'javascript'
    } else if (base.endsWith('.ligo')) {
      return 'pascaligo'
    } else if (base.endsWith('.mligo')) {
      return 'cameligo'
    } else if (base.endsWith('.jsligo')) {
      return 'javascript'
    } else {
      return defaultModeDetector(filePath)
    }
})

const makeContextMenu = (contextMenu, projectManager) => node => {
  if(!node) {
    return []
  }

  // hide "Open" menu item when it`s a folder
  if (node.children) {
    const menus = [...contextMenu]
    const index = findIndex(contextMenu, item => item?.text === 'Open')
    if(index !== -1) {
      menus.splice(index, 2)
    }
    return menus
  }

  if (node.name.endsWith('.json')) {
    const { dir, name } = projectManager.path.parse(node.path)
    if (!name.endsWith('.abi')) { // && dir.endsWith(path.join('build', 'contracts'))
      const cloned = [...contextMenu]
      cloned.splice(projectManager.remote ? 3 : 5, 0, {
        text: 'Deploy',
        onClick: () => projectManager.deploy(node),
      }, null)
      return cloned
    }
  } else if (node.name.endsWith('.sol') && !projectManager.remote) {
    const cloned = [...contextMenu]
    cloned.splice(projectManager.remote ? 3: 5, 0, {
      text: 'Compile',
      onClick: () => projectManager.compile(node.path),
    }, null)
    return cloned
  }
  return contextMenu
}

Workspace.defaultProps = {
  ProjectManager,
  compilerManager,
  ProjectToolbar,
  CompilerTerminal,
  addLanguages: addSolidityLanguage,
  makeContextMenu,
}

export default Workspace
