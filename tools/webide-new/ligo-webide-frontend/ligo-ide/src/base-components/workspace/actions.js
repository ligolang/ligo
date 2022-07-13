import redux from '~/base-components/redux'
import notification from '~/base-components/notification'
import fileOps from '~/base-components/file-ops'

import BaseProjectManager from './ProjectManager/BaseProjectManager'

export class ProjectActions {
  constructor() {
    this.history = null
    this.newProjectModal = null
    this.openProjectModal = null
    this.workspace = null
  }

  get codeEditor () {
    return this.workspace?.codeEditor?.current
  }

  async newProject () {
    await this.processNewProject(this.newProjectModal)
  }

  async openProject () {
    await this.processNewProject(this.openProjectModal)
  }

  async processNewProject (modal) {
    try {
      const created = await modal.openModal()
      const { id, author } = created
      redux.dispatch('ADD_PROJECT', {
        type: 'local',
        project: created
      })
      this.history.push(`/${author}/${id}`)
    } catch (e) {
      console.log(e)
    }
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

  // TODO remove project from local storage
  async removeProject ({ id, name, type }) {
    const selected = redux.getState().projects.get('selected')
    if (selected && selected.get('id') === id) {
      redux.dispatch('SELECT_PROJECT', { project: undefined })
      const author = 'local'
      this.history.replace(`/${author}`)
    }
    redux.dispatch('REMOVE_PROJECT', { id })
    await fileOps.deleteDirectory('.workspaces/' + id)

    let notificationTitle = 'Remove Project Successful'
    let notificationDescription = `Project <b>${name}</b> is removed`
    if (type == 'delete') {
      notificationTitle = 'Delete Project Successful'
      notificationDescription = `You have permanently delete project <b>${name}</b>`
    }
    notification.info(notificationTitle, notificationDescription)
  }
}

export default new ProjectActions()
