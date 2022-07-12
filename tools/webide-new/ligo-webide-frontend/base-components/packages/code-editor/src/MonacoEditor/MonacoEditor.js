import React, { Component } from 'react'
import PropTypes from 'prop-types'

import * as monaco from 'monaco-editor'
import throttle from 'lodash/throttle'

import modelSessionManager from './modelSessionManager'
import registerThemes from './languages/registerThemes'

export default class MonacoEditor extends Component {
  static propTypes = {
    readOnly: PropTypes.bool,
    modelSession: PropTypes.object.isRequired,
    onCommand: PropTypes.func.isRequired,
    onChange: PropTypes.func.isRequired,
    onChangeDecorations: PropTypes.func.isRequired
  }

  componentDidMount () {
    registerThemes()

    this.throttledLayoutEditor = throttle(this.layoutEditor, 500)
    this.monacoEditor = this.createEditorWith(this.props.modelSession.model)

    this.monacoEditor.onDidChangeModelDecorations(this.props.onChangeDecorations)

    this.throttledLayoutEditor()
    // api.bridge.send('languageClient.create')

    if (modelSessionManager.projectManager.onEditorReady) {
      modelSessionManager.projectManager.onEditorReady(this.monacoEditor, this)
    }
  }

  shouldComponentUpdate (props) {
    if (props.modelSession !== this.props.modelSession) {
      if (this.props.modelSession.model) {
        this.props.modelSession.viewState = this.monacoEditor.saveViewState()
      }
      props.modelSession.recoverInEditor(this.monacoEditor)

      this.throttledLayoutEditor()
      // $.bottomBar.updatePosition(this.monacoEditor.getPosition())
    }

    if (props.editorConfig !== this.props.editorConfig) {
      const { fontFamily, fontSize, ligatures } = this.props.editorConfig
      this.monacoEditor.updateOptions({
        fontFamily: fontFamily || 'Hack',
        fontSize: fontSize || '13px',
        fontLigatures: Boolean(ligatures)
      })
    }

    return false
  }

  componentWillUnmount () {
    this.monacoEditor.dispose()
  }

  layoutEditor = () => {
    if (this.monacoEditor) {
      this.monacoEditor.layout()
    }
  }

  createEditorWith (model) {
    const { theme, editorConfig = {}, readOnly = false } = this.props
    const monacoEditor = monaco.editor.create(document.getElementById('monaco-editor'), {
      model,
      theme: theme || 'vs',
      wordwrap: 'wordWrapColumn',
      fontFamily: editorConfig.fontFamily || 'Hack',
      fontSize: editorConfig.fontSize || '13px',
      fontLigatures: Boolean(editorConfig.ligatures),
      scrollBeyondLastLine: false,
      glyphMargin: true,
      readOnly: readOnly,
      domReadOnly: readOnly
    })
    modelSessionManager.editor = monacoEditor
    monacoEditor.onDidChangeModelContent(() => {
      this.props.onChange()
      this.props.modelSession.saved = false
      modelSessionManager.projectManager.onFileChanged()
    })
    monacoEditor.onDidChangeCursorPosition(({ position }) => {
      // $.bottomBar.updatePosition(position)
    })
    monacoEditor.onDidBlurEditorWidget(() => {
      // monacoEditor.focus()
    })
    monacoEditor.onMouseDown(() => {
      monacoEditor.focus()
    })

    this.addCommands(monacoEditor)

    window.addEventListener('resize', () => {
      this.throttledLayoutEditor()
    })
    return monacoEditor
  }

  quickCommand () {
    if (this.monacoEditor) {
      this.monacoEditor.getAction('editor.action.quickCommand').run()
    }
  }

  addCommands (monacoEditor) {
    monacoEditor.addCommand(
      monaco.KeyMod.CtrlCmd | monaco.KeyMod.Shift | monaco.KeyCode.KEY_P,
      () => this.quickCommand()
    )

    monacoEditor.addCommand(
      monaco.KeyMod.CtrlCmd | monaco.KeyCode.KEY_S,
      () => this.props.onCommand('save')
    )
    monacoEditor.addCommand(
      monaco.KeyMod.CtrlCmd | monaco.KeyCode.KEY_W,
      () => this.props.onCommand('close-current-tab')
    )
    monacoEditor.addCommand(
      monaco.KeyMod.CtrlCmd | monaco.KeyCode.US_COMMA,
      () => this.props.onCommand('project-settings')
    )
    monacoEditor.addCommand(
      monaco.KeyMod.WinCtrl | monaco.KeyCode.Tab,
      () => this.props.onCommand('next-tab')
    )
    monacoEditor.addCommand(
      monaco.KeyMod.WinCtrl | monaco.KeyMod.Shift | monaco.KeyCode.Tab,
      () => this.props.onCommand('prev-tab')
    )
  }

  render () {
    return (
      <div id='monaco-editor' className='w-100 h-100 bg2' />
    )
  }
}
