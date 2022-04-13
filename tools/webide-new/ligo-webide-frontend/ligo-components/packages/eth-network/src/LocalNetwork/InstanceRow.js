import React, { PureComponent } from 'react'

import {
  IconButton,
  DeleteButton,
} from '@obsidians/ui-components'

import nodeManager, { NodeButton, NodeStatus } from '@obsidians/eth-node'
import notification from '@obsidians/notification'
import redux from '@obsidians/redux'

import instanceChannel from './instanceChannel'

export default class InstanceRow extends PureComponent {
  constructor (props) {
    super(props)
    this.button = React.createRef()
  }

  componentDidMount () {
    if (this.props.data.running) {
      const { Name, Labels } = this.props.data
      const name = Name.substr(process.env.PROJECT.length + 1)
      const { version, chain } = Labels
      this.button.current?.setState({ lifecycle: 'started' })
      const params = { id: `dev.${name}`, version }
      nodeManager.updateLifecycle('started', params)
      this.props.onNodeLifecycle(name, 'started', params)
    }
  }

  renderStartStopBtn = (name, version, chain) => {
    if (this.props.lifecycle !== 'stopped' && this.props.runningInstance !== name) {
      return null
    }
    return (
      <NodeButton
        ref={this.button}
        name={name}
        version={version}
        chain={chain}
        onLifecycle={(lifecycle, params) => this.props.onNodeLifecycle(name, lifecycle, params)}
      />
    )
  }

  renderVersionBtn = version => {
    return (
      <div className='btn btn-sm btn-secondary'>
        <i className='fas fa-code-merge mr-1' />
        <b>{version}</b>
      </div>
    )
  }

  renderChainBtn = chain => {
    return (
      <div className='btn btn-sm btn-secondary'>
        <b>{chain}</b>
      </div>
    )
  }

  renderBlockNumber = name => {
    if (this.props.runningInstance !== name) {
      return null
    }
    return <NodeStatus />
  }

  renderConfigButton = () => {
    if (!this.props.configButton) {
      return null
    }
    return (
      <IconButton
        color='transparent'
        className='mr-1 text-muted'
        onClick={() => this.props.onOpenConfig(this.props.data)}
        icon='fas fa-cog'
      />
    )
  }

  deleteInstance = async name => {
    if (this.props.lifecycle !== 'stopped' && this.props.runningInstance === name) {
      notification.error('Unable to Delete', 'Please stop the instance first if you want to delete it.')
      return
    }
    await instanceChannel.invoke('delete', name)
    redux.dispatch('DELETE_INSTANCE', { name })
    this.props.onRefresh()
  }

  render () {
    const { data } = this.props
    const name = data.Name.substr(process.env.PROJECT.length + 1)
    const labels = data.Labels

    return (
      <tr className='hover-flex'>
        <td><div className='flex-row align-items-center'>{name}</div></td>
        <td>{this.renderStartStopBtn(name, labels.version, labels.chain)}</td>
        <td>{this.renderVersionBtn(labels.version)}</td>
        <td>{this.renderChainBtn(labels.chain)}</td>
        <td>{this.renderBlockNumber(name)}</td>
        <td align='right'>
          <div className='d-flex align-items-center justify-content-end'>
            {this.renderConfigButton()}
            <DeleteButton onConfirm={() => this.deleteInstance(name)} />
          </div>
        </td>
      </tr>
    )
  }
}
