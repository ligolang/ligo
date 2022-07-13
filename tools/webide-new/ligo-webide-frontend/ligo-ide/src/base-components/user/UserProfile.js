import React, { PureComponent } from 'react'

import {
  Media
} from '~/base-components/ui-components'

import { withRouter } from 'react-router'

import platform from '~/base-components/platform'

class UserProfile extends PureComponent {
  state = {
    loaded: false
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
        <Media body />
      </Media>
    )
  }
}

export default withRouter(UserProfile)
