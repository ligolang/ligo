import React from 'react';
import { useDispatch, useSelector } from 'react-redux';
import styled from 'styled-components';

import { AppState } from '../redux/app';
import { ChangeEntrypointAction, CompileState } from '../redux/compile';
import { Group, Input, Label } from './inputs';

const Container = styled.div``;

export const CompilePaneComponent = () => {
  const dispatch = useDispatch();
  const entrypoint = useSelector<AppState, CompileState['entrypoint']>(
    state => state.compile.entrypoint
  );

  return (
    <Container>
      <Group>
        <Label htmlFor="entrypoint">Entrypoint</Label>
        <Input
          id="entrypoint"
          value={entrypoint}
          onChange={ev =>
            dispatch({ ...new ChangeEntrypointAction(ev.target.value) })
          }
        ></Input>
      </Group>
    </Container>
  );
};
