import React, { PureComponent } from 'react'

function TerminalInputIcon (props) {
  if (props.executing) {
    return (
      <div key='xterm-icon-executing' className='xterm-form-icon xterm-form-icon-executing hover-block'>
        <span className='hover-hide'>
          <i className='fas fa-spin fa-spinner' />
        </span>
        <span className='hover-show' onClick={props.onStop}>
          <i className='fas fa-stop-circle' />
        </span>
      </div>
    )
  }

  return (
    <div key='xterm-icon-executed' className='xterm-form-icon'>
      <i className='far fa-greater-than' />
    </div>
  )
}

export default class TerminalInput extends PureComponent {
  state = {
    value: '',
    executing: false
  }

  constructor (props) {
    super(props)
    this.state = {
      value: '',
      executing: false
    }
    this.inputRef = React.createRef()
  }

  focus = () => this.inputRef.current.focus()

  onChange = event => {
    this.setState({ value: event.target.value })
  }

  onSubmit = async event => {
    event.preventDefault()
    if (this.state.executing) {
      return
    }
    this.setState({ executing: true })
    try {
      await this.props.onSubmit(this.state.value)
    } catch (e) {
      console.warn(e)
    } finally {
      this.setState({ value: '', executing: false })
    }
  }

  render () {
    const { value, executing } = this.state

    return (
      <form className='xterm-form' onSubmit={this.onSubmit}>
        <TerminalInputIcon executing={executing} onStop={this.props.onStop} />
        <input
          ref={this.inputRef}
          className='xterm-input'
          readOnly={executing}
          spellCheck='false'
          autoComplete='off'
          autoCorrect='off'
          autoCapitalize='off'
          value={value}
          onChange={this.onChange}
        />
      </form>
    )
  }
}