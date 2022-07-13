import React from 'react'
import Card from './index'
import Table from '../Table'

export default function TableCard ({ title, right, noPadding, tableSm, tableScroll, TableHead, children }) {
  return (
    <Card title={title} right={right} noPadding={noPadding}>
      <Table tableSm={tableSm} tableScroll={tableScroll} TableHead={TableHead}>
        {children}
      </Table>
    </Card>
  )
}