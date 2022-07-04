import React, {FC} from 'react';
import { useDispatch, connect } from 'react-redux';
import styled from 'styled-components';
import { protocolType } from '../../redux/compile';

import { ChangeEntrypointAction, ChangeStorageAction, UseNetworkAction, UseSignerAction, networkType, signerType, ChangeProtocolAction } from '../../redux/deploy';
import { CheckboxComponent } from '../form/checkbox';
import { AccessFunctionLabel, Group, HGroup, Input, Label, Textarea } from '../form/inputs';
import { Option, SelectCommand } from '../form/select';

const Container = styled.div``;

const Checkbox = styled(CheckboxComponent)`
  margin-right: 0.3em;
`;

const Hint = styled.span`
  font-style: italic;
  font-size: 0.8em;
`;

interface stateTypes {
  entrypoint?: string;
  storage?: string;
  useNetwork?: string
}

const DeployPaneComponent:FC<stateTypes> = (props) => {

  const {entrypoint, storage} = props
  let {useNetwork} = props

  const dispatch = useDispatch();

  const setSigner = (isSelected) => {
    if(isSelected && useNetwork !== networkType.Mainnet){
      dispatch({ ...new UseSignerAction(signerType.Sign) })
    } else {
      dispatch({ ...new UseSignerAction(signerType.Beacon) })
    }
  }

  return (
    <Container>
      <Group>
      <Label htmlFor="protocol">Choose a protocol (used for compilation)</Label>
        <SelectCommand
          id="protocol-select"
          value={protocolType.Jakarta}
          onChange={ev =>
            dispatch({ ...new ChangeProtocolAction(ev.target.value) })
          }>
          <Option value={protocolType.Jakarta}>Jakarta</Option>
        </SelectCommand>
      <Label htmlFor="storage">Choose a Network</Label>
      <SelectCommand
          id="command-select"
          value={useNetwork}
          onChange={network => {
            useNetwork = network
              dispatch({ ...new UseNetworkAction(network) })
              if (useNetwork !== networkType.Mainnet) {
                setSigner(true)
              } else {
                setSigner(false)
              }
          }}
        >
          <Option value={networkType.Jakartanet}>Jakartanet</Option>
          <Option value={networkType.Mainnet}>Mainnet</Option>
        </SelectCommand>
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
      {useNetwork && ( useNetwork === networkType.Jakartanet) &&
      <HGroup>
        <Checkbox
          checked={true}
          onChanged={(value) => setSigner(value)}
        ></Checkbox>
        <Label htmlFor="tezbridge">
          We'll sign for you
          <br />
          <Hint>Got your own key? Deselect to sign with Beacon</Hint>
        </Label>
      </HGroup>
      }
    </Container>
  );
};

function mapStateToProps(state) {
  const { deploy } = state
  return { 
    entrypoint: deploy.entrypoint,
    storage: deploy.storage,
    useNetwork: deploy.network,
    useSigner: deploy.signer
   }
}

export default connect(mapStateToProps, null)(DeployPaneComponent)
