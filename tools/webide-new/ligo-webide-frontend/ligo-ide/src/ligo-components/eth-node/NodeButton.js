import React, { PureComponent } from 'react'

import nodeManager from './nodeManager'

export default class NodeButton extends PureComponent {
  constructor (props) {
    super(props)
    this.state = {
      lifecycle: 'stopped'
    }
  }

  onLifecycle = async (lifecycle, params) => {
    await nodeManager.updateLifecycle(lifecycle, params)
    if (this.props.onLifecycle) {
      this.props.onLifecycle(lifecycle, params)
    }
  }

  start = async () => {
    if (this.state.lifecycle !== 'stopped') {
      return
    }
    this.setState({ lifecycle: 'starting' })
    this.onLifecycle('starting')

    try {
      const params = await nodeManager.start({
        name: this.props.name,
        version: this.props.version,
        chain: this.props.chain,
      }, this)
      await this.onLifecycle('started', params)
      this.setState({ lifecycle: 'started' })
    } catch (e) {
      this.setState({ lifecycle: 'stopped' })
      this.onLifecycle('stopped')
    }
  }

  stop = async () => {
    const lifecycle = this.state.lifecycle
    if (lifecycle === 'stopping' || lifecycle === 'stopped') {
      return
    }
    this.setState({ lifecycle: 'stopping' })
    this.onLifecycle('stopping')

    await nodeManager.stop({
      name: this.props.name,
      version: this.props.version,
      chain: this.props.chain,
    })

    this.setState({ lifecycle: 'stopped' })
    this.onLifecycle('stopped')
  }

  renderStartBtn () {
    return (
      <div key='node-btn-stopped-no-miner' className='btn-group btn-group-sm'>
        <button type='button' className='btn btn-success' onClick={this.start}>
          <i className='fas fa-play mr-1' />Start
        </button>
      </div>
    )
  }

  renderStartingBtn () {
    return (
      <div key='node-btn-starting' className='hover-inline'>
        <button type='button' className='btn btn-sm btn-transparent hover-inline-hide'>
          <i className='fas fa-circle-notch fa-spin mr-1' />Starting
        </button>
        <button type='button' className='btn btn-sm btn-danger hover-inline-show' onClick={this.stop}>
          <i className='fas fa-stop mr-1' />Stop
        </button>
      </div>
    )
  }

  render () {
    switch (this.state.lifecycle) {
      case 'stopped':
        return this.renderStartBtn()
      case 'started':
        return (
          <div key='node-btn-stop' className='btn btn-sm btn-danger' onClick={this.stop}>
            <i className='fas fa-stop mr-1' />Stop
          </div>
        )
      case 'starting':
        return this.renderStartingBtn()
      case 'stopping':
        return (
          <div key='node-btn-stopping' className='btn btn-sm btn-transparent'>
            <i className='fas fa-circle-notch fa-spin mr-1' />Stopping
          </div> 
        )
      default:
        return null
    }
  }
}
