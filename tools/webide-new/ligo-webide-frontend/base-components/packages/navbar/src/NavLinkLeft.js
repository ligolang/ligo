import React, { PureComponent } from 'react'

import { withRouter } from 'react-router'
import { NavLink } from 'react-router-dom'

import NavLinkContent from './NavLinkContent'
import NavDropdown from './NavDropdown'

class NavLinkLeft extends PureComponent {
  static defaultProps = {
    onClickItem: () => { },
  }

  onToggle = event => {
    const { route, selected, history, location } = this.props
    let url = `/${route}`
    if (selected.id) {
      url = `/${route}/${selected.id}`
    }
    const match = location.pathname.startsWith(url)
    if (!match) {
      history.push(url)
    }
    return !match
  }

  onClickItem = item => {
    if (item.onClick) {
      item.onClick()
    } else {
      const { history } = this.props
      this.props.onClickItem(item.id, item)
      history.push(`/${item.route}/${item.id || ''}`)
    }
  }

  render() {
    const { route, title, selected, dropdown, icon, contextMenu, disable } = this.props

    let url = `/${route}`
    if (selected.id) {
      url = `/${route}/${selected.id}`
    }
    return (
      <NavLink
        to={url}
        className='nav-link d-flex p-0'
        style={{ width: 273 }}
        activeClassName='active'
      >
        <NavDropdown
          route={route}
          selected={selected.id || ''}
          list={dropdown}
          onToggle={this.onToggle}
          onClickItem={this.onClickItem}
          icon={icon}
          disable={disable}
          contextMenu={contextMenu}
        >
          <NavLinkContent
            title={title}
            selected={selected.name}
            icon={icon}
            width='100%'
          />
        </NavDropdown>
      </NavLink>
    )
  }
}

export default withRouter(NavLinkLeft)
