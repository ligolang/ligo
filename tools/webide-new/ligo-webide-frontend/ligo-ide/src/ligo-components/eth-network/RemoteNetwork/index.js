import React, { PureComponent } from 'react'

import notification from '~/base-components/notification'

import networkManager from '../networkManager'
import DefaultRemoteNetworkInfo from './RemoteNetworkInfo'

export default class RemoteNetwork extends PureComponent {
  constructor (props) {
    super(props)
    this.state = {
      info: null,
      status: null,
    }
  }

  componentDidMount () {
    this.refresh()
    this.h = setInterval(() => this.refreshBlock(), 5000)
  }

  componentDidUpdate (prevProps) {
    if (
      this.props.url !== prevProps.url ||
      this.props.networkId !== prevProps.networkId
    ) {
      this.refresh()
    }
  }

  componentWillUnmount () {
    if (this.h) {
      clearInterval(this.h)
    }
    this.h = undefined
  }

  async refresh () {
    this.setState({ info: null, status: null })
    if (!networkManager.sdk) {
      return
    }
    this.refreshBlock()
    const networkId = this.props.networkId
    try {
      const info = await networkManager.sdk?.networkInfo()
      if (this.props.networkId === networkId) {
        this.setState({ info })
      }
    } catch {}
  }

  async refreshBlock () {
    if (!networkManager.sdk) {
      return
    }
    try {
      const networkId = this.props.networkId
      const status = await networkManager.sdk?.getStatus()
      if (this.props.networkId === networkId) {
        this.setState({ status })
      }
    } catch (error) {
      console.warn(error)
      if (error.message.startsWith('missing response')) {
        notification.error('Internet Disconnected')
        if (this.h) {
          clearInterval(this.h)
        }
        this.h = undefined
      }
      this.setState({ status: null })
    }
  }

  render () {
    const {
      networkId,
      url,
      EditButton,
      RemoteNetworkInfo = DefaultRemoteNetworkInfo,
    } = this.props
    const { status, info } = this.state

    return (
      <div className='d-flex flex-1 flex-column overflow-auto'>
        <RemoteNetworkInfo
          networkId={networkId}
          url={url}
          EditButton={EditButton}
          info={info}
          status={status}
        />
        <div className='d-flex flex-fill'>
          <div className='col-12 p-0 border-top-black'>
          </div>
        </div>
      </div>
    )
  }
}


