import React, { useState } from 'react'
import { UncontrolledTooltip } from '~/base-components/ui-components'

import { useHistory } from 'react-router-dom'

const formatAddress = address => <code>{address.substr(0, 12)}...{address.substr(address.length - 6, address.length)}</code>
const getUrl = (address, route = 'account') => `/${route}/${address}`

export default function Address ({ addr, route, redirect = true, displayText, showTooltip = true }) {
  if (!addr) {
    return null
  }
  const history = useHistory()
  const [id] = useState(`tooltip-address-${addr.replace(/\W/g, '')}-${Math.floor(Math.random() * 1000)}`)
  const hash = displayText ? displayText : formatAddress(addr)
  const url = getUrl(addr, route)
  let text
  if (redirect) {
    text = (
      <a href="javascript:void(0)" onClick={() => history.push(url)} className='text-body small' id={id}>
        {hash}
      </a>
    )
  } else {
    text = (
      <span className='text-body small' id={id} style={{ cursor: 'default' }}>
        {hash}
      </span>
    )
  }
  return <>
    <div>{text}</div>
    {
      showTooltip &&
      <UncontrolledTooltip trigger='hover' delay={0} target={id} key={id}>
        { addr }
      </UncontrolledTooltip>
    }
  </>
}
