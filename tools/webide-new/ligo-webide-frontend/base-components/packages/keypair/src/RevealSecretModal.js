import React, { PureComponent } from 'react'

import {
  Modal,
  Badge,
} from '@obsidians/ui-components'

import keypairManager from './keypairManager'

export default class RevealSecretModal extends PureComponent {
  constructor (props) {
    super(props)
    this.state = { address: '', secret: '', secretName: '' }
    this.modal = React.createRef()
  }

  async openModal (keypair) {
    this.setState({ address: keypair.address })
    try {
      const rawSecret = await keypairManager.getSecret(keypair.address)
      const { secret, secretName } = await this.props.kp.importKeypair(rawSecret)
      this.setState({ secret, secretName })
    } catch (e) {
      console.warn(e)
    }
    this.modal.current.openModal()
  }

  render () {
    const { address, secret } = this.state
    const secretName = this.state.secretName || this.props.secretName

    return (
      <Modal
        ref={this.modal}
        title={`View ${secretName}`}
        textCancel='Close'
      >
        <div className='row align-items-center'>
          <div className='col-2'>
            <Badge pill color='info' className='ml-1'>Address</Badge>
          </div>
          <div className='col-10 pl-0'>
            <code className='user-select small'>{address}</code>
          </div>
        </div>
        <div className='row align-items-center'>
          <div className='col-2'>
            <Badge pill color='success' className='ml-1'>{secretName}</Badge>
          </div>
          <div className='col-10 pl-0'>
            <code className='user-select small'>{secret}</code>
          </div>
        </div>
      </Modal>
    )
  }
}
