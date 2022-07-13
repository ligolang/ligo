import React, { PureComponent } from 'react'
import classnames from 'classnames'

import {
  UncontrolledButtonDropdown,
  DropdownToggle,
  DropdownMenu,
  DropdownItem,
} from '~/base-components/ui-components'

import keypairManager from './keypairManager'
import KeypairManagerModal from './KeypairManagerModal'
export default class KeypairSelector extends PureComponent {
  constructor(props) {
    super(props)
    this.state = {
      loading: true,
      keypairs: [],
    }
    this.modal = React.createRef()
  }

  componentDidMount() {
    this.setState({ loading: true })
    keypairManager.loadAllKeypairs().then(this.updateKeypairs)
    this.listenKeypairChange = keypairManager.onUpdated(this.updateKeypairs)
  }

  componentWillUnmount() {
    this.listenKeypairChange && this.listenKeypairChange()
  }

  componentDidUpdate(prevProps) {
    if (prevProps.filter !== this.props.filter) {
      this.updateKeypairs(this.allKeypairs || [])
    }
    if (prevProps.enabled !== this.props.enabled) {
      this.updateKeypairs(this.allKeypairs || [])
    }
  }

  updateKeypairs = allKeypairs => {
    this.allKeypairs = allKeypairs

    const { filter, enabled } = this.props
    const keypairs = filter ? allKeypairs.filter(filter) : allKeypairs
    const enabledKeypairs = enabled ? keypairs.filter(enabled) : keypairs

    if (!this.state.keypairs.length && enabledKeypairs.length) {
      this.props.onSelected(enabledKeypairs[0].address)
    }
    if (this.state.keypairs.length && !enabledKeypairs.length) {
      this.props.onSelected()
    }
    if (enabledKeypairs.length && !enabledKeypairs.find(k => k.address === this.props.selected)) {
      this.props.onSelected(enabledKeypairs[0].address)
    }
    this.setState({ loading: false, keypairs })
  }

  openManager = () => {
    this.modal.current.openModal()
  }

  renderItems = (iconComponent) => {
    const { networkManager } = require('~/ligo-components/eth-network')
    if (this.state.loading) {
      return <DropdownItem key='loading' disabled><i className='fas fa-spin fa-spinner mr-1' />Loading...</DropdownItem>
    }

    if (!this.state.keypairs.length) {
      const none = `(No ${this.props.noneName})`
      return <DropdownItem key='none' disabled>{none}</DropdownItem>
    }

    const keypairs = [...this.state.keypairs]
    return keypairs.map(k => {
      const disabled = !this.props.enabled(k)
      return (
        <DropdownItem
          key={`k-${k.address}`}
          disabled={disabled}
          active={this.props.selected === k.address}
          onClick={() => this.props.onSelected(k.address)}
        >
          <div>
            {iconComponent}{k.name}
            <div className={classnames('small code', !disabled && 'text-muted')}>{networkManager?.sdk.utils.formatAddress(k.address)}</div>
          </div>
        </DropdownItem>
      )
    })
  }

  render() {
    const {
      size,
      color = 'default',
      icon = 'fas fa-key',
      title,
      modalTitle,
      secretName,
      modifyNameDisabled,
      deletionDisabled,
      selected,
    } = this.props

    const selectedKeypair = this.state.keypairs.find(k => k.address === selected)
    const selectedName = selectedKeypair && (selectedKeypair.name || selectedKeypair.address)

    let iconComponent = null
    if (this.props.icon) {
      iconComponent = <span key='icon' className='mr-1'><i className={icon} /></span>
    }
    return <>
      <UncontrolledButtonDropdown direction='up'>
        <DropdownToggle
          size='sm'
          color={color}
          className='rounded-0 px-2 text-nowrap overflow-hidden text-overflow-dots'
          style={{ maxWidth: 240 }}
        >
          <span className='d-inline-block w-3'>{iconComponent}</span>
          {selectedName || '(none)'}
        </DropdownToggle>
        <DropdownMenu right className={classnames({ 'dropdown-menu-sm': size === 'sm' })}>
          <DropdownItem header>
            {title}
          </DropdownItem>
          {this.renderItems(iconComponent)}
          <DropdownItem divider />
          <DropdownItem onClick={this.openManager}>
            <i className='fas fa-cog mr-1' />
            {modalTitle}...
          </DropdownItem>
        </DropdownMenu>
      </UncontrolledButtonDropdown>
      <KeypairManagerModal
        ref={this.modal}
        secretName={secretName}
        modifyNameDisabled={modifyNameDisabled}
        deletionDisabled={deletionDisabled}
      />
    </>
  }
}
