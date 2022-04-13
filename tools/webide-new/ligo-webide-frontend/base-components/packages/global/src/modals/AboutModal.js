import React, { PureComponent } from 'react'

import { Modal } from '@obsidians/ui-components'
import fileOps from '@obsidians/file-ops'
import platform from '@obsidians/platform'

import globalModalManager from './globalModalManager'

export default class AboutModal extends PureComponent {
  constructor (props) {
    super(props)
    this.modal = React.createRef()
    this.state = {
      debugCounter: 0,
      appVersion: ''
    }
  }

  async componentDidMount () {
    globalModalManager.aboutModal = this
    const appVersion = await fileOps.current.getAppVersion()
    this.setState({ appVersion })
  }

  openModal () {
    this.modal.current.openModal()
  }

  handleClick () {
    if (platform.isWeb) {
      return
    }
    if (this.state.debugCounter > 20) {
      try {
        console.info('Dev Tools Toggled')
        window.require('electron').BrowserWindow.getFocusedWindow().toggleDevTools()
      } catch (e) {}
      this.setState({ debugCounter: 0 })
    } else {
      this.setState({ debugCounter: this.state.debugCounter + 1 })
    }
  }

  render () {
    const { appVersion } = this.state

    return (
      <Modal
        ref={this.modal}
        title='About'
        textCancel='Close'
      >
        <div className='d-flex flex-column align-items-center justify-content-center'>
          <img src={this.props.icon} style={{ background: 'transparent', width: '100px' }} onClick={() => this.handleClick()} />
          <p className='mt-3'><span className='h4'><b>{process.env.PROJECT_NAME}</b></span> v{appVersion}</p>

          <h5 className='small-caps mt-4'>contact us</h5>
          <p>Website: <a href='#' onClick={() => fileOps.current.openLink('https://www.obsidians.io')}>https://www.obsidians.io</a></p>
          {this.props.children}
        </div>
      </Modal>
    )
  }
}
