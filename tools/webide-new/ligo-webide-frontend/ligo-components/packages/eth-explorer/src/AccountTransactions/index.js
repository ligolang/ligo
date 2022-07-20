import React, { PureComponent } from 'react'

import {
  TableCard,
  Badge,
} from '@obsidians/ui-components'

import { networkManager } from '@obsidians/eth-network'

import TransactionRow from './TransactionRow'

export default class AccountTransactions extends PureComponent {
  state = {
    hasMore: true,
    loading: true,
    txs: [],
    page: 0,
    total: -1,
    size: 10,
    hide: false,
    error: '',
  }

  componentDidMount () {
    this.refresh(this.props.account)
  }

  componentDidUpdate (prevProps) {
    if (prevProps.account !== this.props.account) {
      this.refresh(this.props.account)
    }
  }

  refresh = async account => {
    this.setState({ txs: [], loading: true, page: 0, error: '' })
    const { total, list: txs, noExplorer } = await networkManager.sdk.getTransactions(account.address, 0, this.state.size)
    if (noExplorer) {
      this.setState({ hide: true })
      return
    }
    if (Array.isArray(txs)) {
      this.setState({
        txs,
        total,
        page: 1,
        hasMore: total ? txs.length < total : txs.length === this.state.size,
      })
    } else {
      this.setState({ error: txs })
    }
    this.setState({ loading: false })
  }

  loadMore = async () => {
    this.setState({ loading: true, error: '' })
    const { total, list: txs, noExplorer } = await networkManager.sdk.getTransactions(this.props.account.address, this.state.page, this.state.size)
    if (noExplorer) {
      this.setState({ hide: true })
      return
    }
    if (Array.isArray(txs)) {
      this.setState({
        txs: [...this.state.txs, ...txs],
        total,
        page: this.state.page + 1,
        hasMore: total ? (this.state.txs.length + txs.length) < total : txs.length === this.state.size,
      })
    } else {
      this.setState({ error: txs })
    }
    this.setState({ loading: false })
  }

  renderTableBody = () => {
    const { TransactionRow } = this.props
    const rows = this.state.txs.map(tx => (
      <TransactionRow key={`tx-${tx.hash}`} tx={tx} owner={this.props.account.address} />
    ))

    if (this.state.loading) {
      rows.push(
        <tr key='txs-loading' className='bg-transparent'>
          <td align='middle' colSpan={8}>
            <i className='fas fa-spin fa-spinner mr-1' />Loading...
          </td>
        </tr>
      )
    } else if (this.state.error) {
      rows.push(
        <tr key='txs-loadmore' className='bg-transparent'>
          <td align='middle' colSpan={8}>
            {this.state.error}
          </td>
        </tr>
      )
    } else if (!this.state.txs.length) {
      rows.push(
        <tr key='txs-loadmore' className='bg-transparent'>
          <td align='middle' colSpan={8}>
            No Transactions Found
          </td>
        </tr>
      )
    } else if (this.state.hasMore) {
      rows.push(
        <tr key='txs-loadmore' className='bg-transparent'>
          <td align='middle' colSpan={8}>
            <span className='btn btn-sm btn-secondary' onClick={this.loadMore}>Load More</span>
          </td>
        </tr>
      )
    }

    return rows
  }


  render () {
    const TransactionHeader = this.props.TransactionHeader
    if (this.state.hide) {
      return null
    }
    const total = Math.max(0, this.state.total) || ''
    return (
      <TableCard
        title={
          <div className='d-flex flex-row align-items-end'>
            <h4 className='mb-0'>Transactions</h4>
            <Badge pill className='ml-1 mb-1'>{total}</Badge>
          </div>
        }
        tableSm
        TableHead={<TransactionHeader />}
      >
        {this.renderTableBody()}
      </TableCard>
    )
  }
}

const TransactionHeader = () => (
  <tr>
    <th style={{ width: '10%' }}>time</th>
    <th style={{ width: '8%' }}>block</th>
    <th style={{ width: '17%' }}>tx hash</th>
    <th style={{ width: '17%' }}>from</th>
    <th style={{ width: '17%' }}>to</th>
    <th style={{ width: '8%', textAlign: 'right' }}>value</th>
    <th style={{ width: '8%', textAlign: 'right' }}>gas used</th>
    <th style={{ width: '15%', textAlign: 'right' }}>fee</th>
  </tr>
)

AccountTransactions.defaultProps = {
  TransactionHeader,
  TransactionRow,
}