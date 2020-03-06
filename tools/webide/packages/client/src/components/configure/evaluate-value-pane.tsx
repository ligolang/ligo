import React from 'react';
import { useDispatch, useSelector } from 'react-redux';
import styled from 'styled-components';

import { AppState } from '../../redux/app';
import { ChangeEntrypointAction, EvaluateValueState } from '../../redux/evaluate-value';
import { Group, Input, Label } from '../form/inputs';

const Container = styled.div``;

export const EvaluateValuePaneComponent = () => {
  const dispatch = useDispatch();
  const entrypoint = useSelector<AppState, EvaluateValueState['entrypoint']>(
    state => state.evaluateValue.entrypoint
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
