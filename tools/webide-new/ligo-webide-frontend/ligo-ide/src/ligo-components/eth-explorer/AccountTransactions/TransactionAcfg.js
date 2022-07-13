import React from 'react'

import { Badge } from '~/base-components/ui-components'

import Address from './Address'

export default function ({ tx }) {
  if (!tx.totalSupply) {
    return (
      <div className='d-flex flex-row align-items-center'>
        <small>{tx.assetName} ({tx.assetID})</small>
      </div>
    )
  }
  const totalSupply = `${(tx.totalSupply / 10 ** tx.decimals).toFixed(tx.decimals)} ${tx.unitName}`
  return (
    <div className='d-flex flex-row align-items-center'>
      <div className='flex-1 overflow-hidden'>
        <small className='mr-2'>{tx.assetName} ({tx.assetID}) by:</small>
        <Address addr={tx.creator} />
      </div>
      <Badge pill color='success'>
        {totalSupply}
      </Badge>
    </div>
  )
}