import React from 'react'

import { Badge } from '@obsidians/ui-components'

import Address from './Address'

export default function ({ tx, owner }) {
  if (!tx.amount) {
    return (
      <div className='d-flex flex-row align-items-center'>
        <small>{tx.assetName} ({tx.assetID})</small>
      </div>
    )
  }
  const amount = `${(tx.amount / 10 ** tx.decimals).toFixed(tx.decimals)} ${tx.unitName}`
  return (
    <div className='d-flex flex-row align-items-center'>
      <div className='flex-1 overflow-hidden'>
        <small>{tx.assetName} ({tx.assetID})</small>
      </div>
      <div className='flex-1 overflow-hidden'>
        <Address addr={tx.from} />
      </div>
      <div className='mx-3 text-muted'>
        <i className='fas fa-long-arrow-alt-right' />
      </div>
      <div className='flex-1 overflow-hidden'>
        <Address addr={tx.to} />
      </div>
      <Badge pill color={tx.from === owner ? 'danger' : 'success'}>
        {amount}
      </Badge>
    </div>
  )
}
