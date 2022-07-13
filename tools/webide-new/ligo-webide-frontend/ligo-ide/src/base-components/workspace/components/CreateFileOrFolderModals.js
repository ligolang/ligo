import React, { PureComponent } from 'react'

import {
  Modal,
  DebouncedFormGroup
} from '~/base-components/ui-components'

import notification from '~/base-components/notification'
import actions from '../actions'

export default class CreateFileOrFolderModals extends PureComponent {
  constructor(props) {
    super(props)
    this.state = {
      type: 'file',
      loading: false,
      name: '',
      baseName: '',
      basePath: ''
    }
    this.modal = React.createRef()
    this.input = React.createRef()
  }

  openCreateFileModal = ({ basePath, baseName }) => {
    this.setState({ type: 'file', loading: false, name: '', baseName, basePath })
    setTimeout(() => this.input.current?.focus(), 100)
    this.modal.current.openModal()
  }

  openCreateFolderModal = ({ basePath, baseName }) => {
    this.setState({ type: 'folder', loading: false, name: '', baseName, basePath })
    setTimeout(() => this.input.current?.focus(), 100)
    this.modal.current.openModal()
  }

  onCreate = async () => {
    const { basePath, name } = this.state
    if (this.state.type === 'file') {
      let filePath
      try {
        filePath = await this.props.projectManager.createNewFile(basePath, name)
      } catch (e) {
        notification.error('Cannot Create File', e.message)
        return
      }
      setTimeout(() => {
        actions.workspace.openFile({ path: filePath }, true)
      }, 500)
    } else if (this.state.type === 'folder') {
      try {
        await this.props.projectManager.writeDirectory(basePath, name)
      } catch (e) {
        notification.error('Cannot Create Folder', e.message)
        return
      }
    }
    this.setState({ loading: false })
    this.modal.current.closeModal()
  }

  render () {
    return (
      <Modal
        ref={this.modal}
        title={this.state.type === 'file' ? 'New File' : 'New Folder'}
        textConfirm='Create'
        pending={this.state.loading && 'Creating...'}
        confirmDisabled={!this.state.name}
        onConfirm={this.onCreate}
      >
        <DebouncedFormGroup
          ref={this.input}
          label={<div>Create a new {this.state.type} in <kbd>{this.state.baseName}</kbd></div>}
          placeholder={this.state.type === 'file' ? 'File name' : 'Folder name'}
          maxLength='50'
          value={this.state.name}
          onChange={name => this.setState({ name })}
        />
      </Modal>
    )
  }
}
