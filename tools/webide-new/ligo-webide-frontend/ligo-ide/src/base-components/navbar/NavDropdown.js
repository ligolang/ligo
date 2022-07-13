import React, { Component } from 'react'
import PropTypes from 'prop-types'
import classnames from 'classnames'
import { Menu, Item, useContextMenu, Separator } from 'react-contexify'

import {
  ButtonDropdown,
  DropdownToggle,
  DropdownMenu,
  DropdownItem
} from '~/base-components/ui-components'

export default class NavDropdown extends Component {
  static propTypes = {
    selected: PropTypes.string.isRequired,
    list: PropTypes.array.isRequired,
    onClickItem: PropTypes.func.isRequired,
    icon: PropTypes.string
  }

  state = {
    dropdown: false,
    activeItem: null
  }

  contextMenuHandler = useContextMenu({
    id: `nav-contextmenu-${this.props.route}`
  })

  handleContextMenu = (event) => {
    event.nativeEvent.preventDefault()

    this.contextMenuHandler.show(event.nativeEvent, {
      props: {
        key: 'value'
      }
    })
  }

  onToggle = event => {
    if (this.props.onToggle) {
      if (this.props.onToggle(event)) {
        return
      }
    }

    if (this.props.disable) {
      return
    }

    this.setState({ dropdown: !this.state.dropdown })
  }

  renderDropdownList = list => {
    if (!list.length) {
      return <DropdownItem disabled>(None)</DropdownItem>
    }
    return list.map(this.renderDropdownItem)
  }

  renderDropdownItem = (item, index, listDropMenu) => {
    if (item.divider) {
      return <DropdownItem divider key={`dropdown-item-divider-${index}`} />
    } else if (item.header) {
      return <DropdownItem className='cursor-default' header key={`dropdown-item-header-${index}`}>{item.header}</DropdownItem>
    } else if (item.none) {
      return <DropdownItem disabled key={`dropdown-item-none-${index}`}>(None)</DropdownItem>
    }

    const { id, name, icon } = item
    const isSelected = this.props.selected === id
    const iconClassName = typeof icon === 'function' ? icon(isSelected) : icon || this.props.icon

    return (
      <DropdownItem
        key={`dropdown-item-header-${id}-${index}`}
        className={classnames({ active: isSelected })}
        onClick={event => {
          event.preventDefault()
          this.props.onClickItem(item)
        }}
        onContextMenu={event => {
          const isLocalProject = listDropMenu.find((elem, elIndex) => elem?.header === 'local projects' && (elem['index'] = elIndex))
          const isRemoteProject = listDropMenu.find((elem, elIndex) => elem?.header === 'remote projects' && (elem['index'] = elIndex))
          if (index < isLocalProject?.index || index > isRemoteProject?.index) {
            return null
          }
          event.preventDefault()
          this.handleContextMenu(event)
          this.setState({
            activeItem: item
          })
        }}
      >
        <span key={`dropdown-item-${isSelected}`}>
          <i className={classnames('mr-2', iconClassName)} />
        </span>
        {name}
      </DropdownItem>
    )
  }

  render() {
    const { list, children, right, contextMenu } = this.props
    let menus = []

    if (Array.isArray(contextMenu)) {
      menus = contextMenu
    } else if (typeof contextMenu === 'function') {
      menus = this.activeItem?.id ? [] : contextMenu(this.activeItem?.id) || []
    }

    return (
      <ButtonDropdown
        group={false}
        className='d-flex flex-1 w-100'
        isOpen={this.state.dropdown}
        toggle={this.onToggle}
      >
        <DropdownToggle
          tag='div'
          caret
          className='nav-dropdown-toggle p-0'
          onClick={event => event.preventDefault()}
        >
          {children}
        </DropdownToggle>
        <DropdownMenu right={right} style={{ width: 'fit-content', top: 48, [right ? 'right' : 'left']: 4 }}>
          {this.renderDropdownList(list)}
        </DropdownMenu>
        {
          menus.length > 0 && <Menu animation={false} id={`nav-contextmenu-${this.props.route}`}>
            {
            menus.map(item => item ? <Item key={item.text} onClick={() => item.onClick(this.state.activeItem)}>{item.text}</Item> : <Separator />)
          }
          </Menu>
        }
      </ButtonDropdown>
    )
  }
}
