import React from 'react'
import CacheRoute from 'react-router-cache-route'

import { connect } from '@obsidians/redux'

import Auth from '@obsidians/auth'
import { KeypairButton } from '@obsidians/keypair'
import { TerminalButton } from '@obsidians/workspace'

import { NetworkStatus } from '@obsidians/eth-network'
import { QueueButton } from '@obsidians/eth-queue'
import { AbiStorage } from '@obsidians/eth-contract'
import { CompilerSelectors } from '@obsidians/eth-compiler'

export default connect(['network', 'queue', 'projects', 'uiState'])(function BottomBar (props) {
  const {
    network,
    queue,
    projects,
    uiState,

    mnemonic = true,
    secretName = mnemonic ? 'Private Key / Mnemonic' : 'Private Key',
    chains,

    noNetwork,
  } = props

  const localNetwork = uiState.get('localNetwork')
  let txs
  if (network !== 'dev') {
    txs = queue.getIn([network, 'txs'])
  } else if (localNetwork && localNetwork.lifecycle === 'started') {
    txs = queue.getIn([localNetwork.params.id, 'txs'])
  }

  const selectedProject = projects.get('selected')
  const loaded = selectedProject?.get('loaded')
  let projectButtons
  if (loaded) {
    projectButtons = <>
      <CacheRoute
        path={[`/${Auth.username}/:project`, '/local/:project']}
        render={() => <CompilerSelectors author={selectedProject.get('author')} />}
      />
      <CacheRoute
        path={[`/${Auth.username}/:project`, '/local/:project']}
        component={TerminalButton}
      />
    </>
  }

  return <>
    <KeypairButton mnemonic={mnemonic} secretName={secretName} chains={chains}>
      <div className='btn btn-primary btn-sm btn-flat'>
        <i className='fas fa-key' />
      </div>
    </KeypairButton>
    { !noNetwork && <NetworkStatus /> }
    <QueueButton txs={txs} />
    <AbiStorage>
      <div className='btn btn-default btn-sm btn-flat text-muted'>
        <i className='fas fa-list mr-1' />
        ABI Storage
      </div>
    </AbiStorage>
    <div className='flex-1' />
    {projectButtons}
  </>
})
