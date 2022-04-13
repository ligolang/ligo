import React from 'react'

import { t } from '@obsidians/i18n'

import CenterScreen from './CenterScreen'

export default function (props) {
  const { text = t('loading') } = props
  return (
    <CenterScreen>
      <i className='fas fa-spin fa-spinner mr-2' />{text}
    </CenterScreen>
  )
}
