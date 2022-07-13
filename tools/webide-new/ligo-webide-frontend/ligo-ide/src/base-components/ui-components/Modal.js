import React, { PureComponent } from 'react'
import PropTypes from 'prop-types'
import classnames from 'classnames'

import {
  Modal,
  ModalHeader,
  ModalBody,
  ModalFooter,
  Button,
  Alert
} from 'reactstrap'

export default class BaseModal extends PureComponent {
  static propTypes = {
    size: PropTypes.string,
    overflow: PropTypes.bool,
    fullscreen: PropTypes.bool,
    scrollable: PropTypes.bool,
    title: PropTypes.node,
    textConfirm: PropTypes.node,
    colorConfirm: PropTypes.string,
    onConfirm: PropTypes.func,
    confirmDisabled: PropTypes.bool,
    headerCancelIcon: PropTypes.bool,
    footerCancelIcon: PropTypes.bool,
    ActionBtn: PropTypes.node,
    textActions: PropTypes.node,
    colorActions: PropTypes.array,
    onActions: PropTypes.array,
    textCancel: PropTypes.string,
    colorCancel: PropTypes.string,
    onCancel: PropTypes.func,
    noCancel: PropTypes.bool,
    children: PropTypes.node,
    onAdditionAction: PropTypes.func,
    textAddition: PropTypes.string,
    colorAddition: PropTypes.string,
    onClosed: PropTypes.func
  }

  constructor (props) {
    super(props)
    this.state = {
      isOpen: false,
      error: ''
    }

    this.modal = React.createRef()
  }

  async openModal () {
    document.addEventListener('keydown', this.onKeyDown)
    return new Promise(resolve => {
      this.setState({ isOpen: true }, resolve)
    })
  }

  async closeModal () {
    if (!this.props.onCancel || await this.props.onCancel()) {
    document.removeEventListener('keydown', this.onKeyDown)
    this.setState({ isOpen: false, error: '' })
    }
  }

  onKeyDown = e => {
    let isEsc = false
    if (e.key) {
      isEsc = e.key === 'Escape' || e.key === 'Esc'
    } else {
      e.keyCode === 27
    }
    if (isEsc) {
      this.closeModal()
    }
  }

  showError (error) {
    this.setState({ error })
  }

  hideError () {
    this.setState({ error: '' })
  }

  toggle = () => this.closeModal()

  renderConfirmButton = ({ onConfirm, colorConfirm = 'primary', confirmDisabled, textConfirm = 'Confirm', pending }) => {
    if (!onConfirm) {
      return null
    }

    if (!pending) {
      return (
        <Button
          key='modal-confirm-btn-not-pending'
          color={colorConfirm}
          className='ml-2'
          disabled={!!confirmDisabled}
          onClick={onConfirm}
        >
          {textConfirm}
        </Button>
      )
    }

    return (
      <Button
        key='modal-confirm-btn-pending'
        color={colorConfirm}
        className='ml-2'
        disabled
      >
        <i className='fas fa-spinner fa-pulse'/> {pending}
      </Button>
    )
  }

  render () {
    const {
      size,
      overflow,
      fullscreen,
      scrollable,
      title,
      ActionBtn = null,
      textActions,
      colorActions = [],
      onActions,
      textCancel = 'Cancel',
      colorCancel = 'default',
      noCancel,
      onAdditionAction,
      textAddition = '',
      colorAddition = 'default',
      onClosed = () => {},
      className,
      children,
      headerCancelIcon,
      footerCancelIcon,
    } = this.props

    let errorComponent = null
    if (this.state.error) {
      errorComponent = (
        <Alert color='danger' className='mb-0'>
          <pre className='pre-wrap user-select mb-0'>{this.state.error}</pre>
        </Alert>
      )
    }

    return (
      <Modal
        ref={this.modal}
        isOpen={this.state.isOpen}
        style={{ userSelect: 'none' }}
        className={classnames({
          'modal-fullscreen': fullscreen,
          'modal-dialog-scrollable': scrollable,
        }, size && `modal-${size}`, className)}
        onClosed={onClosed}
      >
        <ModalHeader
          toggle={noCancel ? undefined : this.toggle}
          close={!noCancel ? void 0 :
            <Button
              size='sm'
              color='default'
              className='text-muted'
              tabIndex={-1}
              onClick={this.toggle}
              hidden={headerCancelIcon}
            >
              <i className='fas fa-times' />
            </Button>
          }
        >
          {title}
        </ModalHeader>
        <ModalBody className={classnames(
          'd-flex flex-column', !overflow && !scrollable && 'overflow-auto'
        )}>
          {children}
          {errorComponent}
        </ModalBody>
        <ModalFooter style={{ justifyContent: 'space-between' }}>
          <div>
            {ActionBtn}
            {textActions && textActions.map((t, i) => <Button key={`action-${i}`} color={colorActions && colorActions[i] ? colorActions[i] : 'success'} className='mr-2' onClick={onActions[i]}>{t}</Button>)}
          </div>
          <div>
            { onAdditionAction && textAddition && <Button color={colorAddition} onClick={onAdditionAction}>{textAddition}</Button> }
            { (!noCancel || !footerCancelIcon) && textCancel && <Button hidden={footerCancelIcon} color={colorCancel} onClick={this.toggle}>{textCancel}</Button> }
            {this.renderConfirmButton(this.props)}
          </div>
        </ModalFooter>
      </Modal>
    )
  }
}
