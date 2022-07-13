import React, { PureComponent } from 'react'

import AbiStorageModal from './AbiStorageModal'

export default class AbiStorage extends PureComponent {
  constructor (props) {
    super(props)
    this.modal = React.createRef()
  }

  openModal = () => {
    this.modal.current.openModal()
  }

  render () {
    return <>
      <div onClick={this.openModal}>{this.props.children}</div>
      <AbiStorageModal ref={this.modal} />
    </>
  }
}