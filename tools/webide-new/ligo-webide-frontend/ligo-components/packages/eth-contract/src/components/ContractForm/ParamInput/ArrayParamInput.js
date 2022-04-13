import React, { PureComponent } from 'react'

import {
  MultiSelect,
  Modal,
} from '@obsidians/ui-components'

import keypairManager from '@obsidians/keypair'

import ActionParamFormGroup from '../ActionParamFormGroup'

const optionItemFromValue = (item, type, index) => {
  let icon = null
  if (type === 'tuple') {
    return {
      value: `item-${index}`,
      item,
      type,
      label: <span key={`item-${index}`}><i className='fas fa-code mr-1' />Object</span>
    }
  }

  let label = item.display || (typeof item.value === 'object' ? item.value.raw : item.value)
  label = label.length > 10 ? `${label.substr(0, 8)}...` : label

  if (type === 'address') {
    icon = <i className='fas fa-map-marker-alt mr-1' />
    label = keypairManager.getName(item.display) || label
  }

  return {
    value: `item-${index}`,
    item,
    type,
    label: <span key={`item-${index}`}>{icon}{label}</span>
  }
}

export default class ArrayParamInput extends PureComponent {
  constructor (props) {
    super(props)

    this.modal = React.createRef()
    this.input = React.createRef()

    this.state = {
      values: [], // props.value
      index: '',
      item: {},
      title: '',
      errorInData: false,
    }
  }

  componentDidMount () {
    this.refreshValues()
    keypairManager.onUpdated(this.refreshValues.bind(this))
  }

  refreshValues = () => {
    const { value, type } = this.props
    if (value?.length) {
      const values = value.map((v, index) => optionItemFromValue(v, type.replace(/\[\d*\]/, ''), index))
      this.onChange(values)
    } else {
      this.onChange([])
    }
  }

  enterNewItem = async () => {
    const index = this.state.values.length
    this.setState({ item: {}, index, title: 'Enter a New Item' })
    this.modal.current.openModal()
    // setTimeout(() => this.input.current.focus(), 100)
    return new Promise(resolve => this.onResolve = resolve)
  }

  onClickItem = async value => {
    const index = this.state.values.indexOf(value)
    this.setState({ item: value.item, index, title: 'Modify an Item' })
    this.modal.current.openModal()
    // setTimeout(() => this.input.current.focus(), 100)
    return new Promise(resolve => this.onResolve = resolve)
  }

  onConfirm = () => {
    const { type } = this.props
    const { item, index } = this.state
    this.onResolve(optionItemFromValue(item, type.replace(/\[\d*\]/, ''), index))
    this.setState({ item: {} })
    this.modal.current.closeModal()
  }

  onChange = values => {
    this.setState({ values })

    const value = values.map(v => v.item)
    const raw = values.map(v => v.item.raw)
    const display = values.map(v => v.item.display)
    const error = values.map(v => v.item.error).find(Boolean)

    this.props.onChange(value, { raw, display, empty: !values.length, error })
  }

  render () {
    const { size, label, type, components } = this.props
    const { index } = this.state
    const itemType = type.replace(/\[\d*\]/, '')

    return <>
      <MultiSelect
        size={size}
        prepend={<span key='icon-array-param'><i className='fas fa-code' /></span>}
        append={<span key='icon-array-add'><i className='fas fa-plus' /></span>}
        onClick={this.enterNewItem}
        value={this.state.values}
        onChange={this.onChange}
        onClickLabel={this.onClickItem}
        placeholder='Click to add item...'
      />
      <Modal
        ref={this.modal}
        overflow
        title={this.state.title}
        onConfirm={this.onConfirm}
        confirmDisabled={this.state.errorInData}
      >
        <ActionParamFormGroup
          ref={this.input}
          label={`${label}[${index}]`}
          type={itemType}
          components={components}
          value={this.state.item.value}
          onChange={(value, extra) => this.setState({ item: { value, ...extra } })}
        />
      </Modal>
    </>
  }
}
