import React from 'react'

import {
  TableCard,
  TableCardRow,
  UncontrolledDropdown,
  DropdownToggle,
  DropdownMenu,
  DropdownItem,
} from '@obsidians/ui-components'

import { networkManager } from '@obsidians/eth-network'

export default function AccountBalance ({ account, tokens, history }) {
  const erc20Tokens = tokens?.filter(t => t.type === 'ERC20')

  return (
    <TableCard title='Account' tableScroll>
      <TableCardRow
        name='Balance'
        icon='fas fa-wallet'
        badge={`${new Intl.NumberFormat().format(account.balance)} ${networkManager.symbol}`}
        badgeColor='success'
      />
      {
        Boolean(erc20Tokens?.length) &&
        <TableCardRow
          name='Tokens'
          icon='fas fa-coins'
          right={
            <UncontrolledDropdown>
              <DropdownToggle caret className='badge badge-pill d-flex align-items-center' color='info'>
                {erc20Tokens.length}
              </DropdownToggle>
              <DropdownMenu className='dropdown-menu-sm' style={{ maxHeight: 360 }}>
                <DropdownItem header>token balance</DropdownItem>
                {erc20Tokens.map(t => {
                  const balance = t.balance / 10 ** t.decimals
                  let formattedBalance
                  if (balance > 1e14) {
                    formattedBalance = balance.toExponential()
                  } else {
                    formattedBalance = new Intl.NumberFormat().format(balance)
                  }
                  return (
                    <DropdownItem key={t.address} className='token-dropdown-item' onClick={() => history.push(`/account/${t.address}`) }>
                      <div className='d-flex flex-row align-items-center'>
                        <img src={t.icon} className='token-icon-lg mr-2' />
                        <div className='d-flex flex-column'>
                          <div className='d-flex flex-row justify-content-between align-items-end'>
                            <span>
                              {formattedBalance} <b>{t.symbol}</b>
                            </span>
                            <div className='small ml-1'>{t.name}</div>
                          </div>
                          <div className='small text-alpha-50'><code>{t.address}</code></div>
                        </div>
                      </div>
                    </DropdownItem>
                  )
                })}
              </DropdownMenu>
            </UncontrolledDropdown>
          }
        />
      }
      {
        account.nonce !== undefined &&
        <TableCardRow
          name='Nonce'
          icon='fas fa-hashtag'
          badge={account.nonce}
        />
      }
    </TableCard>
  )
}
