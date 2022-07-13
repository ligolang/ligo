import React, { PureComponent } from 'react'
import fileOps from '~/base-components/file-ops'

import {
  Modal,
  DebouncedFormGroup
} from '~/base-components/ui-components'

import notification from '~/base-components/notification'

import ProjectManager from '../ProjectManager'
import actions from '../actions'

export default class OpenProjectModal extends PureComponent {
  constructor (props) {
    super(props)

    this.state = {
      name: '',
      link: '',
      loading: ''
    }

    this.modal = React.createRef()
    this.nameInput = React.createRef()
    this.linkInput = React.createRef()

    actions.openProjectModal = this
  }

  openModal () {
    this.setState({
      name: '',
      link: '',
      loading: false
    })
    this.forceUpdate()
    this.modal.current.openModal()
    return new Promise(resolve => { this.onConfirm = resolve })
  }

  onOpenProject = async () => {
    this.setState({ creating: true })

    const { name, link } = this.state

    const gistId = this.getGistId(link)

    const obj = await fileOps.loadGistProject(gistId).catch(e => {
      notification.error('Gist load error', e.message)
    })

    if (!obj) {
      this.setState({ creating: false })
      return
    }

    const created = await this.openProject(obj, name)

    if (created) {
      this.modal.current.closeModal()
      this.onConfirm(created)
      this.setState({ name: '', link: '', loading: false })
    } else {
      this.setState({ creating: false })
    }
  }

  getGistId = (str) => {
    const idr = /[0-9A-Fa-f]{8,}/
    const match = idr.exec(str)
    return match ? match[0] : null
  }

  async openProject (obj, name) {
    try {
      const Manager = ProjectManager.Local
      const created = await Manager.openProject(obj, name)
      notification.success('Successful', `New project <b>${name}</b> is loaded.`)
      return created
    } catch (e) {
      notification.error('Cannot Create the Project', e.message)
      return false
    }
  }

  render () {
    return (
      <Modal
        ref={this.modal}
        title='Load project from Gist'
        textConfirm='Load'
        onConfirm={this.onOpenProject}
        pending={this.state.loading && 'Loading...'}
        confirmDisabled={!this.state.name || !this.state.link}
      >
        <DebouncedFormGroup
          ref={this.nameInput}
          label='Project name'
          value={this.state.name}
          onChange={name => this.setState({ name })}
        />
        <DebouncedFormGroup
          ref={this.linkInput}
          label='Gist link'
          value={this.state.link}
          onChange={link => this.setState({ link })}
        />
      </Modal>
    )
  }
}
