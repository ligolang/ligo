import React, { PureComponent } from 'react'
import classnames from 'classnames'

import {
  Screen,
  UncontrolledButtonDropdown,
  ToolbarButton,
  DropdownToggle,
  DropdownMenu,
  DropdownItem,
  FormGroup,
  Label,
  InputGroup,
  Input,
  InputGroupAddon,
  Button,
} from '@obsidians/ui-components'

import { withRouter } from 'react-router-dom'

import notification from '@obsidians/notification'

import FormSection from './components/MarginlessFormSection'

import { networkManager } from '@obsidians/eth-network'

class ContractEvents extends PureComponent {
  state = {
    selected: 0,
    loading: false,
    rangeFrom: '',
    rangeTo: '',
    error: '',
    logs: '',
  }

  selectAction (index) {
    this.setState({
      selected: index,
      loading: false,
      error: '',
      logs: '',
    })
  }

  getEventLogs = async selectedEvent => {
    if (this.state.loading) {
      return
    }

    const latest = await networkManager.sdk.latest()
    let { rangeFrom, rangeTo } = this.state

    if (rangeFrom > latest || rangeTo > latest) {
      notification.error('Invalid Range', `The range cannot exceed the latest (${latest}).`)
      return
    }

    const maxGap = this.props.contract.maxGap
    if (typeof rangeFrom === 'number' && typeof rangeTo !== 'number') {
      rangeTo = rangeFrom + 10 * maxGap - 1
      if (rangeTo > latest) {
        rangeTo = latest
      }
      this.setState({ rangeTo })
    }
    if (typeof rangeTo === 'number' && typeof rangeFrom !== 'number') {
      rangeFrom = rangeTo - (10 * maxGap - 1)
      if (rangeFrom < 0) {
        rangeFrom = 0
      }
      this.setState({ rangeFrom })
    }
    if (typeof rangeTo !== 'number') {
      rangeTo = latest
      rangeFrom = latest - (10 * maxGap - 1)
      if (rangeFrom < 0) {
        rangeFrom = 0
      }
      this.setState({ rangeFrom, rangeTo })
    } else {
      if (rangeFrom >= rangeTo) {
        notification.error('Invalid Range', 'The value of <b>from</b> must be smaller than the value of <b>to</b>.')
        return
      } else if (rangeTo - rangeFrom > (50 * maxGap - 1)) {
        notification.error('Invalid Range', `The range span cannot be larger than ${50 * maxGap}.`)
        return
      }
    }

    this.setState({ loading: true, error: '', logs: [] })

    const { contract } = this.props
    let to = rangeTo
    let from = rangeTo - (maxGap - 1) > rangeFrom ? rangeTo - (maxGap - 1) : rangeFrom
    let hasMore = true
    while (hasMore) {
      let logs
      try {
        logs = await contract.getLogs(selectedEvent, { from, to })
      } catch (e) {
        console.warn(e)
        notification.error('Get Event Log Failed', e.message)
        this.setState({ loading: false, error: e.message })
        return
      }

      await new Promise(resolve => {
        let allLogs = [...this.state.logs, ...logs.reverse()]
        if (allLogs.length > 100) {
          allLogs = allLogs.slice(0, 100)
          hasMore = false
        }
        this.setState({ logs: allLogs }, resolve)
      })

      if (from > rangeFrom) {
        to = from - 1
        from = to - (maxGap - 1) > rangeFrom ? to - (maxGap - 1) : rangeFrom
      } else {
        hasMore = false
      }
    }

    this.setState({ loading: false })
  }

  renderEventSelector = () => {
    const events = this.props.abi
    const selectedEvent = events[this.state.selected] || {}

    return <>
      <UncontrolledButtonDropdown size='sm'>
        <DropdownToggle color='primary' caret className='rounded-0 border-0 px-2 border-right-1'>
          <i className='far fa-calendar-alt' />
          <code className='ml-2 mr-1'><b>{selectedEvent.name}</b></code>
        </DropdownToggle>
        <DropdownMenu>
          <DropdownItem header>events</DropdownItem>
          {events.map((item, index) => (
            <DropdownItem
              key={item.name}
              className={classnames({ active: index === this.state.selected })}
              onClick={() => this.selectAction(index)}
            >
              <code>{item.name}</code>
            </DropdownItem>
          ))}
        </DropdownMenu>
      </UncontrolledButtonDropdown>
      <ToolbarButton
        key={this.state.loading ? 'event-loading' : 'event-query'}
        icon={this.state.loading ? 'fas fa-spin fa-spinner' : 'fas fa-play'}
        tooltip='Get event logs'
        className='border-right-1'
        onClick={() => this.getEventLogs(selectedEvent)}
      />
    </>
  }

  renderLogsTable = () => {
    const events = this.props.abi
    const selectedEvent = events[this.state.selected] || {}
    const columns = selectedEvent.inputs || []
    return (
      <div className='fixed-table'>
        <div>
          <table className='table table-sm table-hover table-striped'>
            <thead>
              <tr>
                <th scope='col'><div>block</div><div>block</div></th>
                {columns.map(({ name, type }) => (
                  <th key={`table-col-${name}`} scope='col'>
                    <div><div style={{ lineHeight: '1.1rem' }}>{name}</div><div style={{ lineHeight: '0.8rem', fontVariant: 'none', fontWeight: '300' }} className='small'>{type}</div></div>
                    <div><div style={{ lineHeight: '1.1rem' }}>{name}</div><div style={{ lineHeight: '0.8rem', fontVariant: 'none', fontWeight: '300' }} className='small'>{type}</div></div>
                  </th>
                ))}
              </tr>
            </thead>
            <tbody>
              {this.renderTableBody(this.state.logs, columns)}
            </tbody>
          </table>
        </div>
      </div>
    )
  }

  renderTableBody = (rows, columns) => {
    if (!rows.length && !this.state.loading) {
      return <tr className='bg-transparent'><td align='middle' colSpan={columns.length + 1}>(no data)</td></tr>
    }
    const history = this.props.history
    const list = rows.map((item, index) => (
      <tr key={`table-row-${index}`}>
        <td><code><small>{item.blockNumber}</small></code></td>
        {columns.map(({ name, type }, index2) => {

          let content = item.args[index2]
          content = content
            ? (content.toString ? content.toString() : JSON.stringify(content))
            : ''

          if (type === 'address') {
            content = (
              <a href='javascript:void(0)' onClick={() => history.push(`/account/${content}`)} className='text-body'>
                {content}
              </a>
            )
          }
          return (
            <td key={`table-item-${index}-${name}`}>
              <code><small>{content}</small></code>
            </td>
          )
        })}
      </tr>
    ))
    if (this.state.loading) {
      list.push(
        <tr key='loading' className='bg-transparent'>
          <td align='middle' colSpan={columns.length + 1}>
            <i className='fas fa-spin fa-spinner mr-1' />Loading...
          </td>
        </tr>
      )
    }
    if (rows.length >= 100) {
      list.push(
        <tr key='too-many-records' className='bg-transparent text-muted'>
          <td align='middle' colSpan={columns.length + 1}>
            (too many records; hide after 100)
          </td>
        </tr>
      )
    }
    return list
  }

  render () {
    const events = this.props.abi
    const maxGap = this.props.contract.maxGap

    if (!events?.length) {
      return <Screen><p>No events found</p></Screen>
    }
    
    const { rangeFrom, rangeTo } = this.state

    let placeholderFrom = `latest - ${10 * maxGap - 1}`
    let placeholderTo = 'latest'
    if (typeof rangeFrom === 'number') {
      placeholderTo = rangeFrom + 10 * maxGap - 1
    } else if (typeof rangeTo === 'number') {
      placeholderFrom = rangeTo - (10 * maxGap - 1)
      if (placeholderFrom < 0) {
        placeholderFrom = 0
      }
    }

    return (
      <div className='d-flex flex-column align-items-stretch h-100'>
        <div className='d-flex border-bottom-1'>
          {this.renderEventSelector()}
        </div>
        <div className='d-flex flex-column flex-grow-1 overflow-auto'>
          <FormSection title='Parameters'>
            <div className='row'>
              <FormGroup className='mb-2 col-12'>
                <Label className='mb-1 small'>Range</Label>
                <InputGroup size='sm'>
                  <Input
                    innerRef={this.input}
                    bsSize='sm'
                    placeholder={placeholderFrom}
                    value={rangeFrom}
                    onChange={event => {
                      const value = Math.abs(parseInt(event.target.value))
                      this.setState({ rangeFrom: isNaN(value) ? '' : value })
                    }}
                  />
                  <InputGroupAddon addonType='prepend'>
                    <Button color='dark' size='sm' className='text-muted'>
                      <i className='fas fa-minus' />
                    </Button>
                  </InputGroupAddon>
                  <Input
                    innerRef={this.input}
                    bsSize='sm'
                    placeholder={placeholderTo}
                    value={rangeTo}
                    onChange={event => {
                      const value = Math.abs(parseInt(event.target.value))
                      this.setState({ rangeTo: isNaN(value) ? '' : value })
                    }}
                  />
                  <InputGroupAddon addonType='append'>
                    <Button color='secondary' size='sm' onClick={() => this.setState({ rangeFrom: '', rangeTo: '' })}>
                      Clear
                    </Button>
                  </InputGroupAddon>
                </InputGroup>
              </FormGroup>
            </div>
          </FormSection>
          <FormSection title='Event Logs' flex={1}>
            {this.renderLogsTable()}
          </FormSection>
        </div>
      </div>
    )
  }
}

export default withRouter(ContractEvents)