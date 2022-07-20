import React, { PureComponent } from 'react'

import { ToolbarButton } from '@obsidians/ui-components'
import BaseProjectManager from '../ProjectManager/BaseProjectManager'

export default class TerminalButton extends PureComponent {
  state = {
    terminal: false,
  }

  componentDidMount () {
    BaseProjectManager.terminalButton = this
  }

  render () {
    const { size = 'sm' } = this.props

    return (
      <ToolbarButton
        id='terminal'
        size={size}
        icon='fas fa-terminal'
        color={this.state.terminal ? 'primary' : 'default'}
        onClick={() => BaseProjectManager.instance.toggleTerminal(!this.state.terminal)}
      />
    )
  }
}
