import React from 'react'
import { Badge } from '@obsidians/ui-components'
import { networkManager } from '@obsidians/eth-network'

export default props => {
  return <Badge pill>{networkManager.sdk?.utils.display(props.value)}</Badge>
}
