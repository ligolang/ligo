import React from 'react';
import { useDispatch, useSelector } from 'react-redux';
import styled from 'styled-components';

import { AppState } from '../../redux/app';
import { protocolType } from '../../redux/compile';
import { ChangeEntrypointAction, ChangeProtocolAction, EvaluateValueState } from '../../redux/evaluate-expr';
import { Group, Input, Label } from '../form/inputs';
import { Option, SelectCommand } from '../form/select';

const Container = styled.div``;


export const EvaluateValuePaneComponent = () => {
  const dispatch = useDispatch();
  const entrypoint = useSelector<AppState, EvaluateValueState['entrypoint']>(
    state => state.evaluateValue && state.evaluateValue.entrypoint
  );
  let protocol = useSelector<AppState, EvaluateValueState['protocol']>(
    state => state.evaluateValue && state.evaluateValue.protocol
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
        <Label htmlFor="entrypoint">Expression</Label>
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
