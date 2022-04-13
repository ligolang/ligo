import React, { PureComponent } from 'react'

import {
  Modal,
} from '@obsidians/ui-components'
import Auth from '@obsidians/auth';

export default class AuthModal extends PureComponent {
  constructor (props) {
    super(props)
    this.state = {
      title: 'Expired',
      textConfirm: 'Authenticate',
      content: 'Authentication is expired, please authenticate it again.',
    }
    this.modal = React.createRef()
    Auth.modal = this
  }

  openModal ({ title, textConfirm, content } = this.props) {
    this.setState({
      title: title || this.state.title,
      textConfirm: textConfirm || this.textConfirm,
      content: content || this.state.content
    })
    this.modal.current.openModal()
    return new Promise(resolve => { this.onConfirm = resolve })
  }

  render () {
    const { title, textConfirm, content } = this.state

    return (
      <Modal
        ref={this.modal}
        title={title}
        textConfirm={textConfirm}
        onConfirm={async () => {
          await Auth.login(this.props.history)
          this.modal.current.closeModal()
        }}
      >
        {content}
      </Modal>
    )
  }
}
