import React, { PureComponent } from 'react'

import {
  IconButton,
} from '@obsidians/ui-components'

import RemoteNetwork from '../RemoteNetwork'

export default class CustomNetwork extends PureComponent {
  constructor (props) {
    super(props)
    this.modal = React.createRef()
  }

  componentDidMount () {
    const { option } = this.props
    if (!option) {
      this.modal.current.openModal()
    }
  }

  render () {
    const {
      networkId,
      option,
      CustomNetworkModal,
      customNetworks,
      placeholder,
    } = this.props

    return <>
      <RemoteNetwork
        networkId={networkId}
        {...option}
        EditButton={
          <IconButton
            color='default'
            className='text-muted'
            icon='fas fa-cog'
            onClick={() => this.modal.current.openModal(option)}
          />
        }
      />
      <CustomNetworkModal
        ref={this.modal}
        customNetworks={customNetworks}
        placeholder={placeholder}
      />
    </>
  }
}
