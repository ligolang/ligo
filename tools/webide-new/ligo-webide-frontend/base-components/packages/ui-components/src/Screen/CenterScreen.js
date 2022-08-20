import React from 'react'

export default function ({ children }) {
  return (
    <div className='d-flex h-100 flex-column'>
      <div className='h-100 d-flex align-items-center justify-content-center'>
        <span className='lead'>
          {children}
        </span>
      </div>
    </div>
  )
}
