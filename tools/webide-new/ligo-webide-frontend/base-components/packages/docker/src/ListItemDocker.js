import React, { PureComponent } from 'react'
import {
  Button,
  ListGroupItem
} from '@obsidians/ui-components'

import fileOps from '@obsidians/file-ops'
import notification from '@obsidians/notification'

import dockerChannel from './DockerChannel'

export default class ListItemDocker extends PureComponent {
  mounted = false
  state = {
    docker: '', // '', 'NONE', 'INSTALLED', 'STARTING', STARTED',
    version: ''
  }

  componentDidMount () {
    this.mounted = true
  }

  componentWillUnmount () {
    this.mounted = false
  }

  refresh = async () => {
    if (this.state.docker === 'STARTING') {
      return
    }

    const version = await dockerChannel.version()
    if (await dockerChannel.check()) {
      this.mounted && this.setState({ docker: 'STARTED', version })
    } else if (version) {
      this.mounted && this.setState({ docker: 'INSTALLED', version })
      if (process.env.OS_IS_LINUX) {
        notification.error('Fail to run docker', 'Make sure the non-root user has privileges to run docker')
      }
    } else {
      this.mounted && this.setState({ docker: 'NONE', version: '' })
    }
  }

  startDocker = async () => {
    this.mounted && this.setState({ docker: 'STARTING' })
    const success = await dockerChannel.launch()
    if (success) {
      this.mounted && this.setState({ docker: 'STARTED' })
    } else {
      this.mounted && this.setState({ docker: 'INSTALLED' })
      notification.error('Fail to run docker', 'Something went wrong when starting Docker, please try again.')
    }
    this.props.onStartedDocker()
  }

  renderIcon = () => {
    switch (this.state.docker) {
      case '':
      case 'NONE':
      case 'INSTALLED':
        return <span key='fail'><i className='fas fa-minus-circle mr-2 text-muted' /></span>
      case 'STARTING':
        return <span key='spin'><i className='fas fa-spin fa-spinner mr-2 text-muted' /></span>
      default:
        return <span key='success'><i className='fas fa-check-circle mr-2 text-success' /></span>
    }
  }

  renderSubtitle = () => {
    switch (this.state.docker) {
      case '':
        return <span>Loading...</span>
      case 'NONE':
        return <span>Docker is required to start a local node.</span>
      default:
        return <span>{this.state.version}</span>
    }
  }

  renderButton = () => {
    switch (this.state.docker) {
      case '':
        return null
      case 'NONE':
        return <Button color='primary' onClick={this.installDocker}>Install</Button>
      case 'INSTALLED':
        if (process.env.OS_IS_LINUX) {
          return (
            <Button color='primary' onClick={() => fileOps.openLink('https://docs.docker.com/engine/install/linux-postinstall')}>
              Need Privileges
            </Button>
          )
        } else {
          return <Button color='primary' onClick={this.startDocker}>Start Docker</Button>
        }
      case 'STARTING':
        return (
          <Button color='primary' disabled>
            <span><i className='fas fa-spin fa-spinner mr-1' /></span>Starting Docker
          </Button>
        )
      default:
        return <Button color='secondary'>Started</Button>
    }
  }

  installDocker = () => {
    if (process.env.OS_IS_LINUX) {
      fileOps.openLink('https://docs.docker.com/engine/install/ubuntu')
    } else {
      fileOps.openLink('https://www.docker.com/products/docker-desktop')
    }
  }

  render () {
    return (
      <ListGroupItem>
        <div className='align-items-center d-flex flex-row justify-content-between'>
          <div>
            <h5>
              {this.renderIcon()}
              <a
                href='#'
                className='text-white'
                onClick={() => fileOps.openLink('https://www.docker.com/')}
              >Docker</a>
            </h5>
            {this.renderSubtitle()}
          </div>
          {this.renderButton()}
        </div>
      </ListGroupItem>
    )
  }
}
