import React from 'react';
import { useDispatch, useSelector } from 'react-redux';
import styled from 'styled-components';

import { AppState } from '../../redux/app';
import {
  ChangeEntrypointAction,
  ChangeStorageAction,
  ChangeToolAction,
  GenerateDeployScriptState,
} from '../../redux/generate-deploy-script';
<<<<<<< HEAD:tools/webide/packages/client/src/components/configure/generate-deploy-script-pane.tsx
import { Tool } from '../../redux/types';
=======
import { Tool, ToolCommand } from '../../redux/types';
>>>>>>> bc2d95d6f6dd4959097e5b6cdb19bce0cc6b3c01:tools/webide/packages/client/src/components/configure/generate-command-pane.tsx
import { AccessFunctionLabel, Group, Input, Label, Textarea } from '../form/inputs';
import { Option, Select } from '../form/select';

const Container = styled.div`
  overflow: auto;
`;

export const GenerateDeployScriptPane = () => {
  const dispatch = useDispatch();

  const tool = useSelector<AppState, GenerateDeployScriptState['tool']>(
    state => state.generateDeployScript.tool
  );

<<<<<<< HEAD:tools/webide/packages/client/src/components/configure/generate-deploy-script-pane.tsx
  const entrypoint = useSelector<AppState, GenerateDeployScriptState['entrypoint']>(
    state => state.generateDeployScript.entrypoint
  );

=======
  const command = useSelector<AppState, GenerateDeployScriptState['command']>(
    state => state.generateDeployScript.command
  );

  const entrypoint = useSelector<AppState, GenerateDeployScriptState['entrypoint']>(
    state => state.generateDeployScript.entrypoint
  );

>>>>>>> bc2d95d6f6dd4959097e5b6cdb19bce0cc6b3c01:tools/webide/packages/client/src/components/configure/generate-command-pane.tsx
  const storage = useSelector<AppState, GenerateDeployScriptState['storage']>(
    state => state.generateDeployScript.storage
  );

  return (
    <Container>
      <Group>
        <Label>Tool</Label>
        <Select
          id="tool"
          value={tool}
          onChange={value => dispatch({ ...new ChangeToolAction(value) })}
        >
          <Option value={Tool.TezosClient}>Tezos Client</Option>
        </Select>
      </Group>
      <Group>
        <AccessFunctionLabel htmlFor="entrypoint"></AccessFunctionLabel>
        <Input
          id="entrypoint"
          value={entrypoint}
          onChange={ev =>
            dispatch({ ...new ChangeEntrypointAction(ev.target.value) })
          }
        ></Input>
      </Group>
      <Group>
        <Label htmlFor="storage">Storage</Label>
        <Textarea
          id="storage"
          rows={9}
          value={storage}
          onChange={ev =>
            dispatch({ ...new ChangeStorageAction(ev.target.value) })
          }
        ></Textarea>
      </Group>
    </Container>
  );
};
