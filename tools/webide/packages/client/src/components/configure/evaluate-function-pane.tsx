import React from 'react';
import { useDispatch, useSelector } from 'react-redux';
import styled from 'styled-components';

import { AppState } from '../../redux/app';
import { protocolType } from '../../redux/compile';
import { ChangeEntrypointAction, ChangeParametersAction, ChangeProtocolAction, EvaluateFunctionState } from '../../redux/evaluate-function';
import { Group, Input, Label, Textarea } from '../form/inputs';
import { Option, SelectCommand } from '../form/select';

const Container = styled.div``;

export const EvaluateFunctionPaneComponent = () => {
  const dispatch = useDispatch();
  const entrypoint = useSelector<AppState, EvaluateFunctionState['entrypoint']>(
    state => state.evaluateFunction && state.evaluateFunction.entrypoint
  );
  const parameters = useSelector<AppState, EvaluateFunctionState['parameters']>(
    state => state.evaluateFunction && state.evaluateFunction.parameters
  );
  let protocol = useSelector<AppState, EvaluateFunctionState['protocol']>(
    state => state.evaluateFunction && state.evaluateFunction.protocol
  );
  return (
    <Container>
      <Group>
        <Label htmlFor="protocol">Choose a protocol (used for compilation)</Label>
        <SelectCommand
          id="protocol-select"
          value={protocol}
          onChange={selectedProtocol => {
            protocol=selectedProtocol
            dispatch({ ...new ChangeProtocolAction(selectedProtocol) })}
          }>
          <Option value={protocolType.Jakarta}>Jakarta</Option>
          <Option value={protocolType.Kathmandu}>Kathmandu</Option>
        </SelectCommand>
        <Label htmlFor="entrypoint">Function name</Label>
        <Input
          id="entrypoint"
          value={entrypoint}
          onChange={ev =>
            dispatch({ ...new ChangeEntrypointAction(ev.target.value) })
          }
        ></Input>
      </Group>
      <Group>
        <Label htmlFor="parameters">Parameters</Label>
        <Textarea
          id="parameters"
          rows={9}
          value={parameters}
          onChange={ev =>
            dispatch({ ...new ChangeParametersAction(ev.target.value) })
          }
        ></Textarea>
      </Group>
    </Container>
  );
};
