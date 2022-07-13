import React, { PureComponent } from 'react'

import {
  Modal,
  UncontrolledButtonDropdown,
  DropdownToggle,
  DropdownMenu,
  DropdownItem
} from '~/base-components/ui-components'

import BaseQueueManager from './BaseQueueManager'

export default class QueueButton extends PureComponent {
  constructor (props) {
    super(props)
    this.state = {
      data: null,
      tx: {}
    }
    BaseQueueManager.button = this

    this.txModal = React.createRef()
    this.allTxsModal = React.createRef()
  }

  openAllTransactionsModal = data => {
    this.allTxsModal.current.openModal()
  }

  openTransaction = tx => {
    this.setState({ tx })
    this.forceUpdate()
    this.txModal.current.openModal()
  }

  renderDropdownItems = (pending, txs, QueueItem) => {
    const pendingItems = pending.map((item, index) => (
      <DropdownItem key={`pending-${index}`} onClick={() => this.openTransaction(item)}>
        <QueueItem {...item} />
      </DropdownItem>
    ))
    if (pendingItems.length) {
      pendingItems.push(<DropdownItem divider key='divider-pending' />)
      pendingItems.unshift(
        <DropdownItem header key='header-pending'>
          <i className='fas fa-spin fa-spinner mr-1' />Pending
        </DropdownItem>
      )
    }

    const txsItems = txs.map((item, index) => (
      <DropdownItem key={`tx-${index}`} onClick={() => this.openTransaction(item)}>
        <QueueItem {...item} />
      </DropdownItem>
    )).slice(0, 15)
    if (!txsItems.length) {
      txsItems.push(<DropdownItem disabled key='disable' >(None)</DropdownItem>)
    }
    // txsItems.push(<DropdownItem divider />)
    txsItems.unshift(
      <DropdownItem header key='header-txs'>
        <i className='fas fa-history mr-1' />Recent Transactions
      </DropdownItem>
    )

    const allTransactions = (
      <DropdownItem onClick={this.openAllTransactionsModal}>
        <div className='d-inline-block w-3'><i className='fas fa-clipboard-check' /></div>
        All Transactions...
      </DropdownItem>
    )

    return [...pendingItems, ...txsItems]
  }

  render () {
    let icon = null
    if (BaseQueueManager.pending.length) {
      icon = <div key='icon-pending' className='d-inline-block w-3 mr-1'><i className='fas fa-spin fa-spinner' /></div>
    } else {
      icon = <div key='icon-no-pending' className='d-inline-block w-3 mr-1'><i className='fas fa-receipt' /></div>
    }

    const { txs, QueueItem, TransactionDetails, ...otherProps } = this.props
    const { tx = {} } = this.state
    const title = tx.data?.title || tx.data?.name

    return <>
      <UncontrolledButtonDropdown direction='up'>
        <DropdownToggle size='sm' color='default' className='rounded-0 px-2 text-muted'>
          {icon}Transactions
        </DropdownToggle>
        <DropdownMenu className='dropdown-menu-sm'>
          {this.renderDropdownItems(BaseQueueManager.pending, txs?.toJS() || [], QueueItem)}
        </DropdownMenu>
      </UncontrolledButtonDropdown>
      <Modal
        ref={this.txModal}
        title={title || 'Transaction'}
        textCancel='Close'
      >
        <TransactionDetails
          {...otherProps}
          tx={tx}
          closeModal={() => this.txModal.current.closeModal()}
        />
      </Modal>
      <Modal
        ref={this.allTxsModal}
        title='All Transactions'
       />
    </>
  }
}
