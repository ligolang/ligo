import React, { PureComponent } from 'react'

import {
  Modal,
  DebouncedFormGroup,
  UncontrolledButtonDropdown,
  DropdownToggle,
  DropdownMenu,
  DropdownItem,
} from '@obsidians/ui-components'

import { BaseProjectManager } from '@obsidians/workspace'
import { utils } from '@obsidians/eth-sdk'

export default class AbiInputModal extends PureComponent {
  constructor (props) {
    super(props)
    this.modal = React.createRef()
    this.nameInput = React.createRef()

    this.state = {
      name: '',
      codeHash: '',
      codeHashEditable: true,
      abi: '',
      projectAbis: null,
      validJson: false,
    }
  }

  openModal = (name, codeHash, abi) => {
    if (name || codeHash) {
      this.setState({ name, codeHash: utils.isValidAddressReturn(codeHash), abi: '', codeHashEditable: !codeHash })
    } else {
      this.setState({ name: '', codeHash: '', abi: '', validJson: false, codeHashEditable: true })
    }
    if (abi) {
      this.onChangeAbi(abi)
    }
    this.loadProjectAbis()
    this.modal.current.openModal()
    setTimeout(() => this.nameInput.current?.focus(), 100)
    return new Promise(resolve => { this.onResolve = resolve })
  }

  loadProjectAbis = async () => {
    const projectAbis = await BaseProjectManager.instance?.readProjectAbis()
    this.setState({
      projectAbis: projectAbis?.map(item => ({ ...item, id: item.pathInProject || item.contractPath }))
    })
  }

  onConfirm = () => {
    this.onResolve({
      name: this.state.name,
      codeHash: this.state.codeHash.toLowerCase(),
      abi: this.state.abi,
    })
    this.setState({ name: '', codeHash: '', abi: '', validJson: false })
    this.modal.current.closeModal()
  }

  onChangeAbi = abi => {
    try {
      JSON.parse(abi)
    } catch (e) {
      this.setState({ abi, validJson: false })
      return
    }
    this.setState({ abi, validJson: true })
  }

  renderAbiSelectionButton = () => {
    const abis = this.state.projectAbis
    if (!abis) {
      return null
    }
    return (
      <UncontrolledButtonDropdown>
        <DropdownToggle caret color='success'>
          Select from the current project
        </DropdownToggle>
        <DropdownMenu className='dropdown-menu-sm' style={{ maxHeight: 240 }}>
          {this.renderAbiDropdownItem(abis)}
        </DropdownMenu>
      </UncontrolledButtonDropdown>
    )
  }

  renderAbiDropdownItem = abis => {
    if (!abis) {
      return null
    }
    return abis.map(item => {
      return (
        <DropdownItem
          key={item.id}
          onClick={() => {
            this.setState({ name: item.name })
            this.onChangeAbi(JSON.stringify(item.abi, null, 2))
          }}
        >
          <b>{item.name}</b>
          <div className='text-muted small'><code>{item.id}</code></div>
        </DropdownItem>
      )
    })
  }

  render () {
    const { name, codeHash, codeHashEditable, validJson } = this.state
    return (
      <Modal
        ref={this.modal}
        scrollable
        title='Enter New ABI'
        ActionBtn={this.renderAbiSelectionButton()}
        onConfirm={this.onConfirm}
        confirmDisabled={!name || !codeHash || !validJson}
      >
        <DebouncedFormGroup
          ref={this.nameInput}
          label='Name'
          value={name}
          onChange={name => this.setState({ name })}
        />
        <DebouncedFormGroup
          label='Code hash / Address'
          value={codeHash}
          onChange={codeHash => this.setState({ codeHash: utils.isValidAddressReturn(codeHash) })}
          // disabled={!codeHashEditable}
        />
        <DebouncedFormGroup
          size='sm'
          label='ABI'
          type='textarea'
          importFromFile='.json'
          placeholder='Please enter the ABI object. Must be a valid JSON array.'
          formGroupClassName='d-flex flex-column flex-grow-1 code'
          inputGroupClassName='flex-grow-1'
          className='h-100 code'
          value={this.state.abi}
          onChange={this.onChangeAbi}
        />
      </Modal>
    )
  }
}
