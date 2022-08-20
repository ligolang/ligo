import React, { PureComponent } from 'react'
import { utils } from '@obsidians/eth-sdk'

import {
  Modal,
  Table,
  IconButton,
  DeleteButton,
  UncontrolledTooltip,
} from '@obsidians/ui-components'

import redux from '@obsidians/redux'
import notification from '@obsidians/notification'

import ViewAbiModal from './ViewAbiModal'
import AbiInputModal from './AbiInputModal'

export default class AbiStorageModal extends PureComponent {
  constructor (props) {
    super(props)

    this.modal = React.createRef()
    this.viewAbiModal = React.createRef()
    this.abiInputModal = React.createRef()

    this.state = {
      loading: false,
      abis: [],
      showPrivateKeys: false,
    }
  }

  openModal = () => {
    this.modal.current.openModal()
    this.refresh()
  }

  refresh () {
    this.setState({ loading: true })
    const abis = redux.getState().abis.toArray()
    this.setState({ abis, loading: false })
  }

  viewAbi = async data => {
    let formattedAbi = ''
    try {
      formattedAbi = JSON.stringify(JSON.parse(data.abi), null, 2)
    } catch {
      notification.error('Failed to parse ABI', 'The saved ABI is not a valid JSON.')
    }
    const { name, codeHash, abi } = await this.abiInputModal.current.openModal(data.name, data.codeHash, formattedAbi)
    redux.dispatch('ABI_UPDATE', [data.codeHash, { name, codeHash, abi }])
    notification.success(
      'ABI Updated',
      `The ABI record is updated in the storage.`
    )
    this.refresh()
  }

  newAbi = async (inputName, inputCodeHash) => {
    const { name, codeHash, abi } = await this.abiInputModal.current.openModal(inputName, inputCodeHash)
    redux.dispatch('ABI_ADD', { name, codeHash, abi })
    notification.success(
      'ABI Added',
      `A new ABI record is added to the storage.`
    )
    this.refresh()
  }

  deleteAbi = async codeHash => {
    redux.dispatch('ABI_DELETE', codeHash)
    notification.info(
      'ABI Deleted',
      `The ABI record is removed from the storage.`
    )
    this.refresh()
  }

  renderTable = () => {
    if (this.state.loading) {
      return (
        <tr key='abis-loading' >
          <td align='middle' colSpan={3}>
            <i className='fas fa-spin fa-spinner mr-1' />Loading...
          </td>
        </tr>
      )
    }
    if (!this.state.abis || !this.state.abis.length) {
      return <tr key='abis-none' ><td align='middle' colSpan={3}>(No ABIs)</td></tr>
    }
    return this.state.abis.map(this.renderAbiRow)
  }

  renderAbiRow = (item, index) => {
    const [codeHash, obj] = item
    const abi = obj.get('abi')
    const name = obj.get('name')
    try {
      abi = JSON.stringify(JSON.parse(abi), null, 2)
    } catch (e) {}
    return (
      <tr key={`abi-${codeHash}`} className='hover-flex'>
        <td>
          <div className='text-overflow-dots'>{name}</div>
        </td>
        <td className='pr-0'>
          <code className='small'>{utils.formatAddress(codeHash)}</code>
        </td>
        <td align='right'>
          <div className='d-flex flex-row justify-content-end hover-show'>
            <IconButton
              color='transparent'
              id={`show-abi-${index}`}
              className='text-muted'
              icon='fas fa-pencil-alt'
              onClick={() => this.viewAbi({ name, codeHash, abi })}
            >
              <UncontrolledTooltip delay={0} placement='top' target={`show-abi-${index}`}>
                Edit
              </UncontrolledTooltip>
            </IconButton>
            <DeleteButton
              onConfirm={() => this.deleteAbi(codeHash)}
            />
          </div>
        </td>
      </tr>
    )
  }

  render () {
    return <>
      <Modal
        ref={this.modal}
        size='lg'
        title='ABI Storage'
        textActions={['New']}
        textCancel='Close'
        onActions={[() => this.newAbi()]}
      >
        <Table
          tableSm
          TableHead={(
            <tr>
              <th style={{ width: '20%' }}>Name</th>
              <th style={{ width: '70%' }}>Code Hash / Address</th>
              <th style={{ width: '10%' }}></th>
            </tr>
          )}
        >
          {this.renderTable()}
        </Table>
      </Modal>
      <ViewAbiModal ref={this.viewAbiModal} />
      <AbiInputModal ref={this.abiInputModal} />
    </>
  }
}