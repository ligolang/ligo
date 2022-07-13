import React from 'react'

import {
  DebouncedInput,
} from '~/base-components/ui-components'

export default function IntegerParamInput ({ size, label, type, value, onChange, placeholder, disabled }) {
  let invalid
  if (value) {
    try {
      BigInt(value)
    } catch {
      invalid = true
    }
    if (type.startsWith('uint') && value < 0) {
      invalid = true
    }
  }
  const feedback = type.startsWith('int') ? 'Invalid integer' : 'Invalid unsigned integer'

  const onChangeValue = value => {
    let number
    try {
      number = BigInt(value)
    } catch (e) {
      onChange(value, { error: new Error(`The entered value of <b>${label}</b> is not an integer number.`) })
      return
    }
    if (type.startsWith('uint') && number < BigInt(0)) {
      onChange(value, { error: new Error(`The entered value of <b>${label}</b> is not a unsigned integer.`) })
      return
    }

    const display = number.toString()
    const raw = number.toString()
    onChange(value, { display, raw, empty: raw === '0' })
  }

  React.useEffect(() => {
    onChangeValue(value)
  }, [])
  
  return (
    <DebouncedInput
      size={size}
      addon={<b>123</b>}
      value={value}
      onChange={onChangeValue}
      placeholder={placeholder}
      disabled={disabled}
      feedback={invalid && feedback}
      invalid={invalid}
    />
  )
}
