/* eslint-disable no-bitwise */
import React, { Component } from "react";
import PropTypes from "prop-types";

import * as monaco from "monaco-editor";
import throttle from "lodash/throttle";
import {
  MonacoLanguageClient,
  CloseAction,
  ErrorAction,
  MonacoServices,
} from "monaco-languageclient";
import { toSocket, WebSocketMessageReader, WebSocketMessageWriter } from "vscode-ws-jsonrpc";
import notification from "~/base-components/notification";

import modelSessionManager from "./modelSessionManager";
import { theme } from "./theme";
import { actions } from "~/base-components/workspace";
import { findNonAsciiCharIndex } from "~/components/validators";
import fileOps from "~/base-components/file-ops";

function createWebSocket() {
  const url = `wss://${process.env.BACKEND_URL}`;
  const webSocket = new WebSocket(url);
  webSocket.onopen = () => {
    const socket = toSocket(webSocket);
    const reader = new WebSocketMessageReader(socket);
    const writer = new WebSocketMessageWriter(socket);

    // Load all file contents and send them to the backend before initializing
    // the language server.
    modelSessionManager.projectManager.loadProjectFileTree().then((fileTree) => {
      const filePromiseMap = new Map();
      loadProjectFileContentsRecursively(fileTree, filePromiseMap).then(() => {
        Promise.all(filePromiseMap.values()).then((contents) => {
          const filenames = Array.from(filePromiseMap.keys());
          const fileMap = new Map();
          for (let i = 0; i < filenames.length; i++) {
            const filename = filenames[i];
            const content = contents[i];
            fileMap.set(filenames[i], contents[i]);
          }
          socket.send(JSON.stringify(Object.fromEntries(fileMap)));

          const languageClient = createLanguageClient({
            reader,
            writer,
          });
          languageClient.start();
          reader.onClose(() => languageClient.stop());
        });
      });
    });
  };
}

async function loadProjectFileContentsRecursively(fileTree, filePromiseMap) {
  if (fileTree.isLeaf) {
    if (isLigoPath(fileTree.key)) {
      filePromiseMap.set(fileTree.key, fileOps.readFile(fileTree.key));
    }
  } else {
    for (let i = 0; i < fileTree.children.length; i++) {
      const child = fileTree.children[i];
      loadProjectFileContentsRecursively(child, filePromiseMap);
    }
  }
}

function isLigoPath(path) {
  return (
    path.endsWith(".mligo") ||
    path.endsWith(".ligo") ||
    path.endsWith(".pligo") ||
    path.endsWith(".jsligo")
  );
}

function createLanguageClient(transports) {
  return new MonacoLanguageClient({
    name: "Sample Language Client",
    clientOptions: {
      // use a language id as a document selector
      documentSelector: ["cameligoext", "jsligoext", "pascaligoext"],
      // disable the default error handler
      errorHandler: {
        error: () => ({ action: ErrorAction.Continue }),
        closed: () => ({ action: CloseAction.DoNotRestart }),
      },
    },
    // create a language client connection from the JSON RPC connection on demand
    connectionProvider: {
      get: () => {
        return Promise.resolve(transports);
      },
    },
  });
}

export default class MonacoEditor extends Component {
  static propTypes = {
    readOnly: PropTypes.bool,
    modelSession: PropTypes.object.isRequired,
    onCommand: PropTypes.func.isRequired,
    onChange: PropTypes.func.isRequired,
    onChangeDecorations: PropTypes.func.isRequired,
  };

  componentDidMount() {
    monaco.editor.defineTheme("ligoide", theme);

    this.throttledLayoutEditor = throttle(this.layoutEditor, 500);
    this.monacoEditor = this.createEditorWith(this.props.modelSession.model);

    this.monacoEditor.onDidChangeModelDecorations(this.props.onChangeDecorations);

    this.props.addLanguagesCallback(this.monacoEditor);

    this.throttledLayoutEditor();

    if (modelSessionManager.projectManager.onEditorReady) {
      modelSessionManager.projectManager.onEditorReady(this.monacoEditor, this);
    }

    // Override go-to-definition logic
    const services = [
      ...this.monacoEditor._instantiationService._parent._services._entries.values(),
    ];
    const textModelService = services.find(
      (x) => Object.getPrototypeOf(x) && "createModelReference" in Object.getPrototypeOf(x)
    );

    // GoToDefinition validates that the range is within the bounds of
    // the text model, so just generate a really big one that will work
    // for any range.
    const navigationTextModel = monaco.editor.createModel(new Array(10000).fill("").join("\n"));

    textModelService.createModelReference = async (uri) => {
      const reference = {
        async load() {
          return navigationTextModel;
        },
        dispose() {},
        textEditorModel: navigationTextModel,
      };
      return {
        object: reference,
        dispose() {},
      };
    };

    const codeEditorService = this.monacoEditor._codeEditorService;
    const openEditorBase = codeEditorService.openCodeEditor.bind(codeEditorService);
    codeEditorService.openCodeEditor = async (input, source) => {
      const result = await openEditorBase(input, source);
      if (result === null) {
        modelSessionManager.nextEditorCursorUpdate = input.options.selection;
        actions.workspace.openFile({
          path: input.resource.path.substring(1),
          remote: true,
          pathInProject: modelSessionManager.projectManager.pathInProject(input.resource.path),
          isLeaf: true,
        });
      }
      return result; // always return the base result
    };
  }

  shouldComponentUpdate(props) {
    if (props.modelSession !== this.props.modelSession) {
      if (this.props.modelSession.model) {
        this.props.modelSession.viewState = this.monacoEditor.saveViewState();
      }
      props.modelSession.recoverInEditor(this.monacoEditor).then(() => {
        if (modelSessionManager.nextEditorCursorUpdate !== null) {
          modelSessionManager.editor.revealRangeInCenterIfOutsideViewport({
            startLineNumber: modelSessionManager.nextEditorCursorUpdate.startLineNumber,
            endLineNumber: modelSessionManager.nextEditorCursorUpdate.endLineNumber,
            startColumn: modelSessionManager.nextEditorCursorUpdate.startColumn,
            endColumn: modelSessionManager.nextEditorCursorUpdate.endColumn,
          });
          modelSessionManager.editor.setPosition({
            lineNumber: modelSessionManager.nextEditorCursorUpdate.startLineNumber,
            column: modelSessionManager.nextEditorCursorUpdate.startColumn,
          });
          modelSessionManager.nextEditorCursorUpdate = null;
        }
      });

      this.throttledLayoutEditor();
    }

    if (props.editorConfig !== this.props.editorConfig) {
      const { fontFamily, fontSize, ligatures } = this.props.editorConfig;
      this.monacoEditor.updateOptions({
        fontFamily: fontFamily || "Hack",
        fontSize: fontSize || "13px",
        fontLigatures: Boolean(ligatures),
      });
    }

    return false;
  }

  componentWillUnmount() {
    this.monacoEditor.dispose();
  }

  layoutEditor = () => {
    if (this.monacoEditor) {
      this.monacoEditor.layout();
    }
  };

  createEditorWith(model) {
    const { theme, editorConfig = {}, readOnly = false } = this.props;
    const monacoEditor = monaco.editor.create(document.getElementById("monaco-editor"), {
      model,
      theme: theme || "vs",
      wordwrap: "wordWrapColumn",
      fontFamily: editorConfig.fontFamily || "Hack",
      fontSize: editorConfig.fontSize || "13px",
      fontLigatures: Boolean(editorConfig.ligatures),
      scrollBeyondLastLine: true,
      glyphMargin: true,
      readOnly,
      domReadOnly: readOnly,
      minimap: {
        enabled: true,
        autohide: true,
        showSlider: "mouseover",
      },
      mouseWheelZoom: true,
    });
    // install Monaco language client services
    MonacoServices.install();

    createWebSocket();

    modelSessionManager.editor = monacoEditor;
    monacoEditor.onDidChangeModelContent((e) => {
      this.props.modelSession.saved = false;
      const nonAsciiInChange = e.changes.find((change) => {
        return findNonAsciiCharIndex(change.text) !== -1;
      });
      if (nonAsciiInChange !== undefined) {
        const { additionIndex, additionColumn, index } = findNonAsciiCharIndex(
          nonAsciiInChange.text
        );
        notification.error(
          "Non ASCII character.",
          `On line ${nonAsciiInChange.range.startLineNumber + additionColumn} column ${
            nonAsciiInChange.range.startColumn + additionIndex
          } you are using a non ASCII character: ${
            nonAsciiInChange.text[index]
          }. Please make sure all the symbols correspond to ASCII chart. Otherwise you may have problems with your project.`
        );
      }
      modelSessionManager.saveCurrentFile();
    });
    monacoEditor.onDidChangeCursorPosition(({ position }) => {
      actions.updatePosition([position.lineNumber, position.column]);
    });
    monacoEditor.onDidBlurEditorWidget(() => {});
    monacoEditor.onMouseDown(() => {
      monacoEditor.focus();
    });

    this.addCommands(monacoEditor);

    window.addEventListener("resize", () => {
      this.throttledLayoutEditor();
    });
    return monacoEditor;
  }

  quickCommand() {
    if (this.monacoEditor) {
      this.monacoEditor.getAction("editor.action.quickCommand").run();
    }
  }

  addCommands(monacoEditor) {
    monacoEditor.addCommand(
      monaco.KeyMod.CtrlCmd | monaco.KeyMod.Shift | monaco.KeyCode.KEY_P,
      () => this.quickCommand()
    );

    monacoEditor.addCommand(monaco.KeyMod.CtrlCmd | monaco.KeyCode.KEY_S, () =>
      this.props.onCommand("save")
    );
    monacoEditor.addCommand(monaco.KeyMod.CtrlCmd | monaco.KeyCode.KeyB, () =>
      this.props.onCommand("compile")
    );
    monacoEditor.addCommand(monaco.KeyMod.CtrlCmd | monaco.KeyCode.KEY_W, () =>
      this.props.onCommand("close-current-tab")
    );
    monacoEditor.addCommand(monaco.KeyMod.CtrlCmd | monaco.KeyCode.US_COMMA, () =>
      this.props.onCommand("project-settings")
    );
    monacoEditor.addCommand(monaco.KeyMod.WinCtrl | monaco.KeyCode.Tab, () =>
      this.props.onCommand("next-tab")
    );
    monacoEditor.addCommand(monaco.KeyMod.WinCtrl | monaco.KeyMod.Shift | monaco.KeyCode.Tab, () =>
      this.props.onCommand("prev-tab")
    );
  }

  render() {
    return <div id="monaco-editor" className="w-100 h-100 bg2" />;
  }
}
