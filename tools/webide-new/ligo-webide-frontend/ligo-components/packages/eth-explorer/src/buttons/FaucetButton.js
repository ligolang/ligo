import React, { PureComponent } from 'react'

import {
  ToolbarButton,
} from '@obsidians/ui-components'

import fileOps from '@obsidians/file-ops'

export default class FaucetButton extends PureComponent {
  claim = async () => {
    let faucetUrl
    if (this.props.network === 'ropsten') {
      faucetUrl = `https://faucet.ropsten.be/`
    } else if (this.props.network === 'rinkeby') {
      faucetUrl = `https://faucet.rinkeby.io/`
    } else if (this.props.network === 'kovan') {
      faucetUrl = `https://faucet.kovan.network/`
    } else if (this.props.network === 'evmostest') {
      faucetUrl = `https://faucet.evmos.org/`
    } else if (this.props.network === 'bnbtest') {
      faucetUrl = `https://testnet.binance.org/faucet-smart`
    } else {
      return
    }

    fileOps.current.openLink(faucetUrl)
  }

  render () {
    if (['ropsten', 'rinkeby', 'kovan'].indexOf(this.props.network) === -1) {
      return null
    }
    return (
      <ToolbarButton
        id='navbar-faucet'
        size='md'
        icon='fas fa-faucet'
        tooltip='Faucet'
        onClick={this.claim}
      />
    )
  }
}
