import React, { PureComponent } from 'react'

import { Modal } from '~/base-components/ui-components'

import globalModalManager from './globalModalManager'

export default class AutoUpdateModal extends PureComponent {
  constructor (props) {
    super(props)
    this.state = {
      version: ''
    }
    this.modal = React.createRef()
  }

  componentDidMount () {
    globalModalManager.autoUpdateModal = this
  }

  openModal = async (version) => {
    this.setState({ version })
    this.modal.current.openModal()
    return new Promise(resolve => { this.onConfirm = resolve })
  }

  confirm = () => {
    this.onConfirm && this.onConfirm(true)
    this.modal.current.closeModal()
  }

  onCancel = () => {
    this.onConfirm && this.onConfirm(false)
    return true
  }

  render () {
    return (
      <Modal
        ref={this.modal}
        title={`${process.env.PROJECT_NAME} v${this.state.version} is now available`}
        textConfirm='Restart and Update'
        onConfirm={this.confirm}
        textCancel='Later'
        onCancel={this.onCancel}
      >
        You can restart and update to the new version. Do you want to do it right now?
      </Modal>
    )
  }
}
