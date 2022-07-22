import modelSessionManager from '../MonacoEditor/modelSessionManager'

import Markdown from './Markdown'
import Settings from './Settings'

export function useBuiltinCustomTabs(tabs) {
  if (tabs.indexOf('markdown') > -1) {
    modelSessionManager.registerCustomTab('markdown', Markdown)
  }
  if (tabs.indexOf('settings') > -1) {
    modelSessionManager.registerCustomTab('settings', Settings, 'Project Settings')
  }
}