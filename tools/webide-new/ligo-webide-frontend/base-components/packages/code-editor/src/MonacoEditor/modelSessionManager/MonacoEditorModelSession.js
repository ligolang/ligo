import fileOps from '@obsidians/file-ops'
import * as monaco from 'monaco-editor'

const delay = ms => new Promise(res => setTimeout(res, ms))

const SEVERITIES = {
  note: 2,
  warning: 4,
  error: 8
}

const decos = {
  classNames: {
    error: 'bg-danger-transparent',
    warning: 'bg-warning-transparent',
    note: 'bg-info-transparent'
  },
  glyphMarginClassNames: {
    error: 'fas fa-times-circle text-danger p-1',
    warning: 'fas fa-exclamation-triangle text-warning p-1',
    note: 'fas fa-circle text-info p-1'
  }
}

function generateAnnotation ({ type, text, row = 1, column = 1, length = 0 }) {
  return {
    range: new monaco.Range(row, column, row, column + length),
    options: {
      isWholeLine: true,
      minimap: type !== 'note',
      // className: EditorSession.decos.classNames[type],
      glyphMarginClassName: decos.glyphMarginClassNames[type]
      // hoverMessage: { value: text }
      // glyphMarginHoverMessage: { value: d.type }
    }
  }
}

export default class MonacoEditorModelSession {
  constructor (model, remote, CustomTab, decorations = []) {
    this._model = model
    this._remote = remote
    this._CustomTab = CustomTab
    this._readOnly = false
    this._showCustomTab = true
    this._viewState = null
    this._saved = true
    this._saving = false
    this._topbar = null
    this.decorations = decorations
    this._public = null
    if (this.filePath.startsWith('/public')) this._public = true
    if (this.filePath.startsWith('/private')) this._public = false
  }

  get model () {
    return this._model
  }
  get filePath () {
    let filePath = decodeURIComponent(this._model.uri.toString().replace('file:///', ''))
    if (this._remote) {
      if (this._public === true) {
        filePath = filePath.replace(/^\/public/, 'public')
        this._model.uri.path = this._model.uri.path.replace(/^\/public/, 'public')
      }
      if (this._public === false) {
        filePath = filePath.replace(/^\/private/, 'private')
        this._model.uri.path = this._model.uri.path.replace(/^\/private/, 'private')
      }
    } else if (process.env.OS_IS_WINDOWS) {
      filePath = fileOps.pathHelper.normalize(filePath.substr(1))
      const [root, others] = filePath.split(':')
      filePath = `${root.toUpperCase()}:${others}`
    }
    return filePath
  }
  get CustomTab () {
    return this._CustomTab
  }

  get value () {
    return this._model?.getValue()
  }
  set value (v) {
    this._model?.setValue(v)
  }

  refreshValue (value) {
    if (this.value !== value) {
      this.value = value
    }
  }

  set saved (v) { this._saved = v }
  get saved () { return this._saved }

  set saving (v) {
    if (this._saving) {
      clearTimeout(this._saving)
    }
    this._saving = setTimeout(() => {
      this._saving = false
    }, 1000)
  }
  get saving () { return Boolean(this._saving) }

  get readOnly () {
    return this._readOnly
  }
  set readOnly (readOnly) {
    this._readOnly = readOnly
  }

  get viewState () {
    return this._viewState
  }
  set viewState (viewState) {
    this._viewState = viewState
  }

  get topbar () {
    return this._topbar
  }
  setTopbar (value) {
    this._topbar = value
  }
  dismissTopbar () {
    this._topbar = null
  }

  async recoverInEditor (monacoEditor) {
    if (!this._model) {
      return
    }

    await delay(10)

    monacoEditor.setModel(this._model)

    if (this.viewState) {
      monacoEditor.restoreViewState(this.viewState)
    }

    monacoEditor.updateOptions({ readOnly: this.readOnly })
  }

  set decorations (decorations = []) {
    if (!this._model) {
      return
    }

    if (!decorations) {
      decorations = []
    }
    const markers = decorations.map(d => this.generateMarkers(d))
    monaco.editor.setModelMarkers(this._model, 'markers', markers)
    this._decorations = this._model.deltaDecorations(this._decorations, decorations.map(generateAnnotation))
  }

  get showCustomTab () {
    return this._showCustomTab
  }
  toggleCustomTab () {
    this._showCustomTab = !this._showCustomTab
  }

  generateMarkers ({ filePath, type, text, row, column, length, notes = [] }) {
    if (!this._model) {
      return
    }

    // const textLines = text.split('\n')
    // if (textLines.length === 4 || textLines.length === 3) {
    //   const [spaces, tildes] = textLines[2].split('^')

    // let startColumn = column
    // let endColumn = column + tildes.length
    // if (tildes.length === 1) {
    //   const wordAtPosition = this._model.getWordAtPosition({ lineNumber: row, column })
    //   if (wordAtPosition) {
    //     startColumn = wordAtPosition.startColumn
    //     endColumn = wordAtPosition.endColumn
    //   }
    // }

    if (typeof row === 'number') {
      if (length) {
        return {
          // code: 'code',
          // source: 'source',
          severity: SEVERITIES[type],
          message: text,
          startLineNumber: row,
          startColumn: column,
          endLineNumber: row,
          endColumn: column + length,
          relatedInformation: [
            // {
            //   resource: monaco.Uri.file(filePath),
            //   message: text,
            //   startLineNumber: row,
            //   startColumn: column,
            //   endLineNumber: row,
            //   endColumn: column + length,
            // }
          ]
        }
      }
      const word = this.model.getWordAtPosition({ column, lineNumber: row })
      return {
        severity: SEVERITIES[type],
        message: text,
        startLineNumber: row,
        startColumn: word ? word.startColumn : column,
        endLineNumber: row,
        endColumn: word ? word.startColumn : column + 1
      }
    }

    return {
      severity: SEVERITIES[type],
      message: text,
      startLineNumber: 1,
      startColumn: 0,
      endLineNumber: 1,
      endColumn: 100,
      relatedInformation: []
    }
  }

  dispose () {
    if (this._model) {
      this._model.dispose()
    }
  }
}
