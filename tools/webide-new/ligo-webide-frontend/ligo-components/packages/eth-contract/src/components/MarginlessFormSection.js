import React from 'react'

export default function MarginlessFormSection (props) {
  const {
    title,
    right,
    children,
    flex = 'none',
  } = props

  return <>
    <div className='d-flex align-items-center justify-content-between px-2 bg-secondary'>
      <div>{title}</div>
      <div className='d-flex'>{right}</div>
    </div>
    <div className='p-relative d-flex flex-column px-3 py-2' style={{ flex }}>
      {children}
    </div>
  </>
}
