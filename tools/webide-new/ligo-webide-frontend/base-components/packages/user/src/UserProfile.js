import React, { PureComponent } from 'react'

import {
  Media,
  Button,
} from '@obsidians/ui-components'

import { withRouter } from 'react-router'

import platform from '@obsidians/platform'
import Auth from '@obsidians/auth'

class UserProfile extends PureComponent {
  state = {
    loaded: false,
  }

  renderAvatar = () => {
    const { profile } = this.props
    const img = new Image()
    img.src = profile?.avatar
    img.crossOrigin = true
    img.onload = () => {
      this.setState({ loaded: true })
    }
    return (
      profile?.avatar && this.state.loaded ?
        <Media
          object
          crossOrigin='true'
          src={profile.avatar}
          className='rounded-circle'
          style={{ width: 100, height: 100 }} />
        :
        <Media
          middle
          key='no-user'
          className='d-flex align-items-center justify-content-center rounded-circle bg-secondary text-muted'
          style={{ width: 100, height: 100 }}
        >
          <i className='fas fa-user-alt fa-3x' />
        </Media>
    )

  }

  renderUserInfo = () => {
    const { profile } = this.props

    if (!profile?.username) {
      return <>
        <Media heading className='text-muted'>
          (not logged in)
        </Media>
        {/* <p className='break-line'>Log in to synchronize your projects on the cloud.</p> */}
        <p className='break-line'>{this.renderLoginButton()}</p>
      </>
    }

    const { username, desc } = profile
    return <>
      <Media heading className='d-flex align-items-end'>
        {username}
        {/* <IconButton
          color='default'
          className='ml-1 text-muted'
          icon='fas fa-pencil-alt'
          onClick={() => {}}
        /> */}
      </Media>
      <p className='break-line'>{this.renderDescription(desc)}</p>
    </>
  }

  renderLoginButton = () => {
    const providers = process.env.LOGIN_PROVIDERS ? process.env.LOGIN_PROVIDERS.split(',') : ['github']
    return providers.map(provider => (
      <Button
        color='primary'
        size='sm'
        key={`user-profile-login-${provider}`}
        onClick={() => Auth.login(this.props.history, provider)}
      >
        <i key='sign-in-${provider}' className='fas fa-sign-in mr-2' />{providers.length > 1 ? `Login ${provider}` : 'Login'}
      </Button>
    ))
  }

  renderDescription = desc => {
    if (desc) {
      return desc
    }
    return <span className='text-muted'>(No description)</span>
  }

  render() {
    if (platform.isDesktop && !process.env.ENABLE_AUTH) {
      return null
    }

    return (
      <Media className='overflow-hidden mb-4'>
        <Media left className='mr-4'>
          {this.renderAvatar()}
        </Media>
        <Media body>
          {this.renderUserInfo()}
        </Media>
      </Media>
    )
  }
}

export default withRouter(UserProfile)