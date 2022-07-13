import React, { PureComponent } from 'react'

import {
  TableCard,
  TableCardRow,
  Badge,
} from '~/base-components/ui-components'

export default function AccountInfo ({ account, tokenInfo }) {
  let tokenInfoRows = null
  if (tokenInfo?.type === 'ERC20') {
    tokenInfoRows = <>
      <TableCardRow
        name='ERC20 Token'
        icon='fas fa-coins'
        badgeColor='primary'
        badge={`${tokenInfo.name} (${tokenInfo.symbol})`}
      />
      <TableCardRow
        name='Total Supply'
        icon='fas fa-box'
        badge={new Intl.NumberFormat().format(tokenInfo.totalSupply / 10 ** tokenInfo.decimals)}
      />
    </>
  }
    
  return (
    <TableCard title='Information'>
      {tokenInfoRows}
      <TableCardRow
        name={account.codeHash ? 'Code Hash' : 'Code'}
        icon='fas fa-code'
        badge={account.codeHash ? account.codeHash : '(None)'}
      />
    </TableCard>
  )
}
