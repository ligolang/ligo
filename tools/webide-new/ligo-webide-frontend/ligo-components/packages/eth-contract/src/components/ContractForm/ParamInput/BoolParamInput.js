import React from 'react'

import {
  ButtonOptions,
} from '@obsidians/ui-components'

export default function BoolParamInput ({ size, value, onChange, placeholder, disabled }) {
  const onChangeValue = value => {
    if (!value || value === 'false') {
      onChange('false', { display: false, raw: false })
    } else {
      onChange('true', { display: true, raw: true })
    }
  }
  
  React.useEffect(() => {
    onChangeValue(value)
  }, [])

  return (
    <div>
      <ButtonOptions
        size={size}
        className='mb-0'
        options={[{ key: 'true', text: 'True' }, { key: 'false', text: 'False' }]}
        selected={value}
        onSelect={onChangeValue}
      />
    </div>
  )
}
