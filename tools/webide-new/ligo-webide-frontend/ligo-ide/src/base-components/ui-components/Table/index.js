import React from 'react'
import classnames from 'classnames'

export default function Table ({ tableSm, tableScroll, TableHead, children }) {
  return (
    <table
      className={classnames(
        `table table-hover table-fixed table-striped`,
        tableSm && 'table-sm',
        tableScroll ? 'overflow-auto' : 'overflow-hidden'
      )}
    >
      <thead>{TableHead}</thead>
      <tbody>
        {children}
      </tbody>
    </table>
  )
}