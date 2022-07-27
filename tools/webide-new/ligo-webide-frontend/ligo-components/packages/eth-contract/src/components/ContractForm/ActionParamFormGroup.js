import React from 'react'
import classnames from 'classnames'

import {
  FormGroup,
  Label,
} from '@obsidians/ui-components'

import ParamInput from './ParamInput'

export default function ActionParamFormGroup ({ size, className, label, type, ...props }) {
  let labelClassName = ''
  if (size === 'sm') {
    labelClassName += 'small'
    if (type === 'tuple') {
      labelClassName += ' mb-0'
    } else {
      labelClassName += ' mb-1'
    }
  }
  return (
    <FormGroup className={classnames(className, size === 'sm' && 'mb-2')}>
      <Label className={labelClassName}>{label}</Label>
      <ParamInput size={size} label={label} placeholder={type} type={type} {...props} />
    </FormGroup>
  )
}
