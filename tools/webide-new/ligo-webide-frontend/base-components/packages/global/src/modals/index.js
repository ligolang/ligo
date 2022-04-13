import React from 'react'
import AutoUpdateModal from './AutoUpdateModal'
import AboutModal from './AboutModal'

export default function GlobalModals (props) {
  return <>
    <AutoUpdateModal />
    <AboutModal icon={props.icon}>
      {props.children}
    </AboutModal>
  </>
}