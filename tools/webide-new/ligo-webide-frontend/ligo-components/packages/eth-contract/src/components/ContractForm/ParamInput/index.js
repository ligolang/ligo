import React from 'react'

import { DebouncedInput } from '@obsidians/ui-components'

import ArrayParamInput from './ArrayParamInput'
import TupleInput from './TupleInput'
import BoolParamInput from './BoolParamInput'
import IntegerParamInput from './IntegerParamInput'
import StringParamInput from './StringParamInput'
import BytesParamInput from './BytesParamInput'
import AddressParamInput from './AddressParamInput'

export default function ParamInput (props) {
  const { type, icon } = props

  if (icon) {
    return <DebouncedInput {...props} addon={<span key={icon.replace(/ /g, '-')}><i className={icon} /></span>} />
  } else if (!type) {
    return `No type given`
  } else if (type.endsWith(']')) {
    return <ArrayParamInput {...props} />
  } else if (type === 'tuple') {
    return <TupleInput {...props} />
  } else if (type === 'bool') {
    return <BoolParamInput {...props} />
  } else if (type.startsWith('int') || type.startsWith('uint')) {
    return <IntegerParamInput {...props} />
  } else if (type === 'string') {
    return <StringParamInput {...props} />
  } else if (type === 'byte' || type.startsWith('bytes')) {
    return <BytesParamInput {...props} />
  } else if (type === 'address' || type === 'FixedHash<20>') {
    return <AddressParamInput {...props} />
  } else {
    return `Unsupported type ${type}`
  }
}
