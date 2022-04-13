import { Base64 } from 'js-base64'
import Auth from '@obsidians/auth'
import fileOps from '@obsidians/file-ops'
import redux from '@obsidians/redux'
import notification from '@obsidians/notification'

import BaseProjectManager from './ProjectManager/BaseProjectManager'

export class ProjectActions {
  constructor() {
    this.history = null
    this.newProjectModal = null
    this.workspace = null
  }

  get codeEditor () {
    return this.workspace?.codeEditor?.current
  }

  async newProject (remote) {
    const created = await this.newProjectModal.openModal(remote)
    const { _id, projectRoot, name } = created
    const author = _id ? Auth.username : 'local'
    const projectId = _id ? name : Base64.encode(projectRoot)
    redux.dispatch('ADD_PROJECT', {
      type: _id ? 'remote' : 'local',
      project: {
        id: projectId,
        author,
        name,
        path: projectRoot,
      }
    })
    this.history.push(`/${author}/${projectId}`)
  }

  async openProject () {
    try {
      const projectRoot = await fileOps.current.chooseFolder()
      const { base } = fileOps.current.path.parse(projectRoot)
      const author = 'local'
      const projectId = Base64.encode(projectRoot)
      redux.dispatch('ADD_PROJECT', {
        type: 'local',
        project: {
          id: projectId,
          author,
          path: projectRoot,
          name: base,
        }
      })
      this.history.push(`/${author}/${projectId}`)
    } catch (e) {}
  }

  newFile () {
    this.workspace?.openCreateFileModal()
  }

  newFolder () {
    this.workspace?.openCreateFolderModal()
  }

  save () { this.codeEditor?.onCommand('save') }
  saveAll () { this.workspace.saveAll() }
  redo () { this.codeEditor?.onCommand('redo') }
  undo () { this.codeEditor?.onCommand('undo') }
  delete () { this.codeEditor?.onCommand('delete') }
  selectAll () { this.codeEditor?.onCommand('selectAll') }

  openTerminal () {
    BaseProjectManager.instance?.toggleTerminal(true)
  }

  async removeProject ({ id, name, type }) {
    const selected = redux.getState().projects.get('selected')
    if (selected && selected.get('id') === id) {
      redux.dispatch('SELECT_PROJECT', { project: undefined })
      const author = Auth.username || 'local'
      this.history.replace(`/${author}`)
    }
    redux.dispatch('REMOVE_PROJECT', { id })
    let notificationTitle = 'Remove Project Successful';
    let notificationDescription = `Project <b>${name}</b> is removed`;
    if (type == 'delete') {
      notificationTitle = 'Delete Project Successful';
      notificationDescription = `You have permanently delete project <b>${name}</b>`;
    }
    notification.info(notificationTitle, notificationDescription)
  }
}

export default new ProjectActions()
