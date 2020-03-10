import * as monaco from 'monaco-editor';
import React, { useEffect, useRef } from 'react';
import { useDispatch, useStore } from 'react-redux';
import styled from 'styled-components';

import { AppState } from '../../redux/app';
import { ChangeCodeAction, ChangeDirtyAction } from '../../redux/editor';
import { ClearSelectedAction } from '../../redux/examples';

const Container = styled.div`
  height: var(--content_height);

  /* This font size is used to calcuate code font size */
  font-size: 0.8em;
`;

export const MonacoComponent = () => {
  let containerRef = useRef(null);
  const store = useStore();
  const dispatch = useDispatch();

  useEffect(() => {
    const cleanupFunc: Array<() => void> = [];
    const { editor: editorState } = store.getState();
    const model = monaco.editor.createModel(
      editorState.code,
      editorState.language
    );

    monaco.editor.defineTheme('ligoTheme', {
      base: 'vs',
      inherit: true,
      rules: [],
      colors: {
        'editor.background': '#eff7ff',
        'editor.lineHighlightBackground': '#cee3ff',
        'editorLineNumber.foreground': '#888'
      }
    });

    monaco.editor.setTheme('ligoTheme');

    const htmlElement = (containerRef.current as unknown) as HTMLElement;
    const fontSize = window
      .getComputedStyle(htmlElement, null)
      .getPropertyValue('font-size');

    const editor = monaco.editor.create(htmlElement, {
      fontSize: parseFloat(fontSize),
      model: model,
      automaticLayout: true,
      minimap: {
        enabled: false
      }
    });

    let shouldDispatchCodeChangedAction = true;

    const { dispose } = editor.onDidChangeModelContent(() => {
      if (shouldDispatchCodeChangedAction) {
        dispatch({ ...new ChangeCodeAction(editor.getValue()) });
        dispatch({ ...new ChangeDirtyAction(true) });
        dispatch({ ...new ClearSelectedAction() });
      }
    });

    cleanupFunc.push(dispose);

    cleanupFunc.push(
      store.subscribe(() => {
        const { editor: editorState }: AppState = store.getState();

        if (editorState.code !== editor.getValue()) {
          shouldDispatchCodeChangedAction = false;
          editor.setValue(editorState.code);
          shouldDispatchCodeChangedAction = true;
        }

        if (editorState.language !== model.getModeId()) {
          if (editorState.language === 'reasonligo') {
            monaco.editor.setModelLanguage(model, 'javascript');
          } else {
            monaco.editor.setModelLanguage(model, editorState.language);
          }
        }
      })
    );

    return function cleanUp() {
      cleanupFunc.forEach(f => f());
    };
  }, [store, dispatch]);

  return <Container id="editor" ref={containerRef}></Container>;
};
