import React, { PureComponent } from 'react'


import ActionParamFormGroup from './ActionParamFormGroup'
import { utils } from '@obsidians/eth-sdk'

export default class ContractForm extends PureComponent {
  constructor (props) {
    super(props)
    this.state = {
      params: props.inputs?.map(({ value }) => ({ value: value || '' })) || []
    }
  }

  componentDidMount () {
    // if (this.props.params) {
    //   this.setState({ params: [...this.props.params] })
    // }
  }

  getParameters = () => {
    const array = []
    const json = {}
    const obj = {}
    let allEmpty = true

    this.props.inputs.forEach(({ name, type }, index) => {
      const param = this.state.params[index]
      const key = name || `(param${index})`
      if (!type) {

        let value
        type === 'address' ? value = param.value.toLowerCase() || '' : value = param.value || ''

        if (value) {
          allEmpty = false
        }
        array.push(value)
        json[key] = value.toString()
        obj[key] = { value }
      } else {
        const { error, raw, display, empty } = param
        if (error) {
          throw error
        }
        if (!empty) {
          allEmpty = false
        }
        
        array.push(raw)
        json[key] = raw
        obj[key] = { type, value: display }
      }
    })
    return { array, json, obj, empty: allEmpty }
  }

  setParamValue = index => (value, extra) => {
    this.state.params[index] = { value: utils.isValidAddressReturn(value), ...extra }
    const params = [...this.state.params]
    this.setState({ params })
  }

  render () {
    const { size, name: methodName, inputs = [], Empty, disabled } = this.props

    if (!inputs.length) {
      return Empty || null
    }

    const params = this.state.params
    const values = inputs.map((_, i) => params[i]?.value || '')

    return (
      <div>
        {inputs.map(({ name, type, components, value }, i) => (
          <ActionParamFormGroup
            key={`${methodName}-${i}`}
            size={size}
            label={name || `(param${i})`}
            type={type}
            components={components}
            value={values[i]}
            onChange={this.setParamValue(i)}
            disabled={disabled || !!value}
          />
        ))}
      </div>
    )
  }
}
