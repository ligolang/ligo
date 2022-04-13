import React, { useState } from 'react'
import classnames from 'classnames'

import {
  Button,
  UncontrolledTooltip
} from 'reactstrap'

export default function IconButton (props) {
  const {
    color,
    id,
    icon = 'fas fa-trash-alt',
    tooltip,
    tooltipPlacement = 'top',
    className,
    onClick,
    children = null
  } = props

  const [iconId] = useState(id || `icon-button-${Math.floor(Math.random() * 10000)}`)
  const tooltipComponent = tooltip && (
    <UncontrolledTooltip trigger='hover' delay={0} placement={tooltipPlacement} target={iconId}>
      {tooltip}
    </UncontrolledTooltip>
  )

  return <>
    <Button
      size='sm'
      color={color}
      id={iconId}
      key={iconId}
      className={classnames('d-flex align-items-center', className)}
      style={{ height: 24, padding: '0 6px' }}
      onClick={onClick}
    >
      <i className={icon} />
      {children}
    </Button>
    {tooltipComponent}
  </>
}
