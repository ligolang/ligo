import React from 'react'

import {
  UncontrolledButtonDropdown,
  DropdownToggle,
  DropdownMenu,
  DropdownItem,
} from '~/base-components/ui-components'

import RpcClientModal from './RpcClientModal'
import networkManager from '../networkManager'

export default function NetworkStatus (props) {
  const rpcModal = React.useRef()

  const { networkId, current: network } = networkManager
  
  const icon = (
    <div key={`network-${networkId}`} className='d-inline-block mr-1'>
      <i className={network?.icon} />
    </div>
  )

  return <>
    <UncontrolledButtonDropdown direction='up'>
      <DropdownToggle size='sm' color='default' className='rounded-0 px-2 text-muted'>
        {icon}{network?.name}
      </DropdownToggle>
      <DropdownMenu className='dropdown-menu-sm'>
        <DropdownItem header>
          <i className='fas fa-hammer mr-1' />network tools
        </DropdownItem>
        <DropdownItem onClick={() => rpcModal.current?.openModal()}>
          RPC Client
        </DropdownItem>
      </DropdownMenu>
    </UncontrolledButtonDropdown>
    <RpcClientModal ref={rpcModal} />
  </>
}
