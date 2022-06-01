import React, { PureComponent } from 'react'
import classnames from 'classnames'

import { Terminal as XTerm } from 'xterm'
import { FitAddon } from 'xterm-addon-fit'
import { SearchAddon } from 'xterm-addon-search'

import chalk from 'chalk'

import { getColor } from '@obsidians/ui-components'
import notification from '@obsidians/notification'
import fileOps from '@obsidians/file-ops'

import 'xterm/css/xterm.css'

import TerminalInput from './TerminalInput'
import './styles.css'

import initTerminalChannel from './lib/initTerminalChannel'
import colorCommand from './lib/colorCommand'

export default class Terminal extends PureComponent {
  constructor(props) {
    super(props)
    this.initialized = false
    this.incompleteLine = ''
    this.termRef = React.createRef()
    this.inputRef = React.createRef()

    this.terminalChannel = initTerminalChannel(this.props.logId, this.props.cwd)
    this.terminalChannel.on('executing', executing => this.setState({ executing }))
    this.terminalChannel.on('data', this.onLogReceived)
  }

  componentDidMount () {
    if (this.props.active) {
      this.initialized = true
      this.createTerm()
    }

    this.autofit = setInterval(() => {
      if (this.term && this.props.active) {
        this.resizeTerm()
      }
    }, 500)
  }

  componentDidUpdate (prevProps) {
    if (!this.initialized && this.props.active) {
      this.initialized = true
      this.createTerm()
    }
    if (!prevProps.active && this.props.active) {
      this.term?.scrollLines(0)
    }
  }

  componentWillUnmount () {
    this.autofit && clearInterval(this.autofit)

    this.stop()
    this.terminalChannel.dispose()

    this.term?.dispose()
  }

  resizeTerm () {
    try {
      this.termFitAddon.fit()
      this.term._core._charSizeService.measure()

      const { cols, rows } = this.term

      if (this.props.active) {
        if (this.cols === cols && this.rows === rows) {
          return
        }
        this.cols = cols
        this.rows = rows
        this.terminalChannel.invoke('resize', { cols: this.props.cols || cols, rows })
      }
    } catch (e) {
      console.warn(e)
    }
  }

  createTerm () {
    const el = this.termRef.current

    if (this.term) {
      return this.term
    }

    el.onmouseup = this.onMouseUpTerm

    const color = getColor('--color-text')
    const bgColor = getColor('--color-bg2')

    const term = new XTerm({
      fontSize: 12,
      fontFamily: this.props.font,
      theme: {
        foreground: color,
        background: bgColor,
        cursor: this.props.interactive ? getColor('--color-text-muted') : bgColor
      }
    })

    this.termFitAddon = new FitAddon()
    this.searchAddon = new SearchAddon()
    term.loadAddon(this.termFitAddon)
    term.loadAddon(this.searchAddon)
    term.open(el)
    try {
      this.termFitAddon.fit()
    } catch (error) {
      console.warn(error)
    }
    this.term = term

    // this.term.attachCustomKeyEventHandler(this.keyboardHandler)
    this.term.onData(this.onData)

    if (this.props.onTermCreated) {
      this.props.onTermCreated(this.term)
    }

    if (this.preActiveMessage) {
      term.write(this.preActiveMessage)
      this.scrollToBottom()
    }

    if (this.props.cmd) {
      this.exec(this.props.cmd, this.props.opt).then(result => {
        if (this.props.onCmdExecuted) {
          this.props.onCmdExecuted(result)
        }
      })
    }

    return this.term
  }

  onData = async data => {
    if (this.props.interactive) {
      await this.terminalChannel.invoke('write', data)
      return
    }

    const buf = Buffer.from(data)
    if (buf.length === 1 && buf[0] === 3) {
      await this.terminalChannel.invoke('write', data)
      // await this.stop()
    }
  }

  onMouseUpTerm = event => {
    const selection = this.term.getSelection()

    if (event.button === 2) {
      navigator.clipboard.writeText(selection)
        .then(() => {
          if (this.props.onCopied) {
            this.props.onCopied()
          }
          notification.success('Copied', 'The selection content is copied to the clipboard.')
          this.term.clearSelection()
          this.focus()
        })
    } else if (!selection) {
      this.focus()
    }
  }

  focus () {
    this.inputRef.current?.focus()
  }

  clearContent () {
    this.term.reset()
  }

  scrollToBottom () {
    if (this.props.active) {
      this.resizeTerm()
      setTimeout(() => this.term?.scrollToBottom(), 300)
    }
  }

  preActiveMessage = ''
  writeToTerminal (message, color) {
    if (color) {
      message = colorCommand(message, color)
    }
    if (this.initialized && this.term) {
      this.term.write(message)
      return
    }
    this.preActiveMessage += message
  }

  exec = async (cmd, config = {}) => {
    if (!this.props.interactive) {
      this.inputRef.current?.setState({ executing: true })
    }

    const result = await this.runCommand(cmd, config)
    if (this.props.onFinished) {
      this.props.onFinished(result)
    }
    if (!this.props.interactive) {
      this.inputRef.current?.setState({ executing: false })
    }
    return result
  }

  getDefaultConfig = async () => {
    const config = { cwd: this.props.cwd }
    if (typeof this.props.getEnv === 'function') {
      config.env = await this.props.getEnv()
    }
    return config
  }

  execAsChildProcess = async (cmd, config) => {
    const mergedConfig = Object.assign(await this.getDefaultConfig(), config)
    await this.terminalChannel.invoke('exec', cmd, mergedConfig)
  }

  onInputSubmit = async (cmd, config) => {
    if (this.props.interactive) {
      return await this.terminalChannel.invoke('write', `${cmd}\n`)
    } else {
      return await this.runCommand(cmd, config)
    }
  }

  runCommand = async (cmd, config) => {
    this.scrollToBottom()

    const mergedConfig = Object.assign(await this.getDefaultConfig(), config)
    this.writeCmdToTerminal(cmd)
    return await this.terminalChannel.invoke('run', cmd, mergedConfig)
  }

  writeCmdToTerminal = (cmd, prefix = '>') => {
    this.writeToTerminal(`${chalk.bold.gray(prefix)} ${colorCommand(cmd)}\n\r`)
  }

  onLogReceived = message => {
    const parsedMessage = this.props.onLogReceived(message)
    this.writeToTerminal(parsedMessage)
  }

  stop = async () => {
    await this.terminalChannel.invoke('kill')
  }

  render () {
    const {
      logId,
      height,
      className,
      Toolbar,
      readOnly,
      input
    } = this.props

    return (
      <div
        className={classnames(`d-flex flex-column w-100 obsidians-terminal bg2`, className)}
        style={{ height }}
      >
        { Toolbar }
        <div className='xterm-wrapper'>
          <div ref={this.termRef} id={`xterm-${logId}`} className='xterm-element' />
        </div>
        { !readOnly && input &&
          <TerminalInput ref={this.inputRef} onSubmit={this.onInputSubmit} onStop={this.stop} />
        }
      </div>
    )
  }
}

Terminal.propTypes = {

}

Terminal.defaultProps = {
  cwd: fileOps.workspace,
  height: '100%',
  font: 'Hack, Menlo, monospace',
  className: '',
  Toolbar: null,
  onLogReceived: message => message
}
