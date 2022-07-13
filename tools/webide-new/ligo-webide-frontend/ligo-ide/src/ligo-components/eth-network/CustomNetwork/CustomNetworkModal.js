import React, { PureComponent } from 'react'

import {
  Modal,
  Table,
  Button,
  IconButton,
  DeleteButton,
} from '~/base-components/ui-components'

import redux from '~/base-components/redux'
import notification from '~/base-components/notification'

import networkManager from '../networkManager'
import NewCustomNetworkModal from './NewCustomNetworkModal'

export default class CustomNetworkModal extends PureComponent {
  constructor (props) {
    super(props)
    this.state = { connecting: '' }
    this.modal = React.createRef()
    this.newConnectionModal = React.createRef()
  }

  openModal = (customNetwork = {}) => {
    this.setState({ connecting: '' })
    this.modal.current?.openModal()
  }

  openNewConnectionModal = (modify, option) => {
    this.newConnectionModal.current?.openModal(modify, option)
  }

  delete = name => {
    redux.dispatch('REMOVE_CUSTOM_NETWORK', name)
  }

  connect = async option => {
    try {
      this.setState({ connecting: option.name })
      const status = await networkManager.updateCustomNetwork(option)
      if (status) {
        redux.dispatch('UPDATE_UI_STATE', { customNetworkOption: option })
        this.modal.current?.closeModal()
        this.setState({ connecting: '' })
        return
      }
    } catch {}
    notification.error('Network Error', 'Failed to connect the network. Make sure you entered a valid url for the node RPC.')
    this.setState({ connecting: '' })
  }

  renderTableBody = () => {
    const connecting = this.state.connecting
    const customNetworks = this.props.customNetworks.toArray()
    if (customNetworks.length) {
      return customNetworks.map(([name, item], i) => (
        <tr key={`custom-network-${i}`} className='hover-flex'>
          <td>{name}</td>
          <td className='text-overflow-dots'>{item.get('url')}</td>
          <td align='right'>
            <div className='d-flex align-items-center justify-content-between'>
              <Button
                key={connecting === name ? `${name}-connecting` : `${name}-connect`}
                size='sm'
                color='success'
                onClick={() => this.connect(item.toJS())}
              >
              {
                connecting === name
                ? <><i className='fas fa-spin fa-spinner mr-1' />Connecting...</>
                : 'Connect'
              }
              </Button>
              {
                connecting !== name &&
                <div className='d-flex hover-show'>
                  <IconButton
                    color='transparent'
                    className='text-muted'
                    onClick={() => this.openNewConnectionModal(true, item.toJS())}
                    icon='fas fa-pencil-alt'
                  />
                  <DeleteButton
                    className='ml-1'
                    onConfirm={() => this.delete(name)}
                  />
                </div>
              }
            </div>
          </td>
        </tr>
      ))
    }
    return <tr key='custom-network-none'><td align='middle' colSpan={3}>(No Custom Networks)</td></tr>
  }

  render () {
    return <>
      <Modal
        ref={this.modal}
        title='Custom Network'
        textActions={['New Connection']}
        onActions={[() => this.openNewConnectionModal()]}
      >
        <Table
          tableSm
          TableHead={(
            <tr>
              <th style={{ width: '20%' }}>name</th>
              <th style={{ width: '55%' }}>rpc url</th>
              <th></th>
            </tr>
          )}
        >
          {this.renderTableBody()}
        </Table>
      </Modal>
      <NewCustomNetworkModal ref={this.newConnectionModal} />
    </>
  }
}
