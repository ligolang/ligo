import React, { PureComponent } from 'react'

import {
  Button,
} from '@obsidians/ui-components'

import Terminal from '@obsidians/terminal'
import notification from '@obsidians/notification'
import { networkManager } from '@obsidians/eth-network'

import { CompilerManager } from './compilerManager'

export default class TruffleTerminal extends PureComponent {
  state = {
    truffleConsole: false,
  }

  tryOpenTruffleConsole = () => {
    if (!networkManager.sdk) {
      notification.error('No Network Detected', 'No Ethereum node instance is running.')
      return
    }
    this.setState({ truffleConsole: true })
    this.notification = notification.info(`Starting Truffle Console...`, '', 0)
    networkManager.onSdkDisposed(this.stopTruffleConsole)
  }

  onCmdExecuted = result => {
    this.notification.dismiss()
    // notification.success(`Truffle Console Started`)
  }

  stopTruffleConsole = () => {
    CompilerManager.truffleTerminal?.stop()
    this.setState({ truffleConsole: false })
  }

  render () {
    const { active, cwd } = this.props
    const { truffleConsole } = this.state
  
    if (!truffleConsole) {
      return (
        <div className='bg-dark p-3 h-100'>
          <h5>Start Truffle Console</h5>
          <Button size='sm' color='primary' onClick={this.tryOpenTruffleConsole}>Start</Button>
        </div>
      )
    }
  
    const cmd = `docker run -it --rm --name truffle-terminal -v "${cwd}":"${cwd}" -w "${cwd}" obsidians/truffle:v5.1.61 truffle console`
    return (
      <Terminal
        ref={ref => (CompilerManager.truffleTerminal = ref)}
        active={active && truffleConsole}
        cwd={cwd}
        logId='compiler-truffle'
        cmd={cmd}
        onCmdExecuted={this.onCmdExecuted}
        opt={{
          resolveOnFirstLog: true,
          stopCommand: 'docker stop -t 1 truffle-terminal'
        }}
        interactive
      />
    )
  }
}