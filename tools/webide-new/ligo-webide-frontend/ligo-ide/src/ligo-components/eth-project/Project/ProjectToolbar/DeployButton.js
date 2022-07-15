import React, { PureComponent } from "react";

import {
  Modal,
  DropdownInput,
  Button,
  UncontrolledTooltip,
  Label,
} from "~/base-components/ui-components";

import notification from "~/base-components/notification";
import { KeypairInputSelector } from "~/base-components/keypair";

import { networkManager } from "~/ligo-components/eth-network";
import { ContractForm, ActionParamFormGroup } from "~/ligo-components/eth-contract";

export default class DeployerButton extends PureComponent {
  constructor(props) {
    super(props);
    this.state = {
      selected: "",
      contracts: [],
      contractName: "",
      contractObj: null,
      constructorAbi: null,
      amount: "",
      signer: "",
      pending: false,
    };
    this.modal = React.createRef();
    this.form = React.createRef();
  }

  componentDidMount() {
    this.props.projectManager.deployButton = this;
  }

  onClick = () => {
    if (this.state.pending) {
      return;
    }
    this.props.projectManager.deploy();
  };

  getDeploymentParameters = async (option, callback, estimate) => {
    this.getConstructorAbiArgs = option.getConstructorAbiArgs || (contractObj => [contractObj.abi]);
    const contractFileNode = option.contractFileNode || option.contracts[0];

    this.setState({
      selected: contractFileNode.path,
      contracts: option.contracts,
    });

    this.modal.current.openModal();
    await this.updateAbi(contractFileNode);
    const options = {};
    networkManager.sdk?.utils.txOptions?.list.forEach(opt => (options[opt.name] = ""));
    this.setState(options);
    this.callback = callback;
    this.estimateCallback = estimate;
  };

  updateContract = async selected => {
    const txOptionObj = Object.fromEntries(
      networkManager.sdk?.utils.txOptions?.list.map(option => [option.name, ""])
    );
    this.setState({ selected, ...txOptionObj });
    const selectedContract = this.state.contracts.find(c => c.path === selected);
    await this.updateAbi(selectedContract);
  };

  updateAbi = async fileNode => {
    const contractName = this.props.projectManager.path.parse(fileNode.path).name;

    let contractObj;
    try {
      contractObj = await this.readContractJson(fileNode);
    } catch (e) {
      notification.error(
        "Built Contract Not Found",
        `Cannot open the file <b>${fileNode.pathInProject}</b>. Please make sure <i>smart contract to deploy</i> is pointting to a valid built contract in the Project Settings.`
      );
      return;
    }

    let constructorAbi;
    try {
      constructorAbi = await this.getConstructorAbi(...this.getConstructorAbiArgs(contractObj));
    } catch (e) {
      notification.error("Built Contract File Error", e.message);
      return;
    }

    this.setState({
      contractName: contractObj.contractName || contractName,
      contractObj,
      constructorAbi,
    });
  };

  readContractJson = async fileNode => {
    const contractJson = await this.props.projectManager.readFile(fileNode.path);

    try {
      return JSON.parse(contractJson);
    } catch (e) {
      throw new Error(`Error in reading <b>${fileNode.pathInProject}</b>. Not a valid JSON file.`);
    }
  };

  getConstructorAbi = (contractAbi, { key = "type", value = "constructor" } = {}) => {
    if (!contractAbi) {
      throw new Error("Error in reading the built contract file.");
    }
    if (!Array.isArray(contractAbi)) {
      throw new Error("Error in reading the built contract file. Field abi is not an array.");
    }
    return contractAbi.find(item => item[key] === value);
  };

  needEstimate = () => {
    if (this.props.skipEstimate) {
      return false;
    }
    if (networkManager.sdk?.utils.txOptions?.list.length) {
      const option = networkManager.sdk.utils.txOptions.list[0];
      const value = this.state[option.name];
      if (!value) {
        return true;
      }
    }
    return false;
  };

  renderTextActions = () => {
    if (this.state.pending) {
    } else if (this.props.skipEstimate) {
      return [`Estimate ${networkManager.sdk?.utils.txOptions?.title}`];
    } else if (this.needEstimate()) {
    } else {
      return ["Re-estimate"];
    }
  };

  prepare = () => {
    let parameters = { array: [], obj: {} };
    if (this.state.constructorAbi) {
      try {
        parameters = this.form.current?.getParameters();
      } catch (e) {
        notification.error("Error in Constructor Parameters", e.message);
        return;
      }
    }

    const { contractName, contractObj, amount, signer } = this.state;
    const options = {};
    networkManager.sdk?.utils.txOptions?.list.forEach(
      opt => (options[opt.name] = this.state[opt.name] || opt.default)
    );

    return [contractObj, { parameters, amount, contractName, signer, ...options }];
  };

  estimate = async () => {
    const args = this.prepare();
    if (!args) {
      return;
    }
    const result = await this.estimateCallback(...args);
    if (result) {
      this.setState(result);
    }
  };

  confirmDeployment = async () => {
    if (this.needEstimate()) {
      this.estimate();
      return;
    }

    const args = this.prepare();
    if (!args) {
      return;
    }
    this.callback(...args);
  };

  closeModal = () => {
    this.setState({ amount: "" });
    this.modal.current.closeModal();
  };

  render() {
    const { signer } = this.props;
    const { contracts, selected, contractName, pending } = this.state;

    let icon = (
      <span key="deploy-icon">
        <i className="fab fa-docker" />
      </span>
    );
    if (pending) {
      icon = (
        <span key="deploying-icon">
          <i className="fas fa-spinner fa-spin" />
        </span>
      );
    }

    const { constructorAbi } = this.state;
    let constructorParameters = null;
    if (constructorAbi) {
      constructorParameters = (
        <>
          {constructorAbi.payable || constructorAbi.stateMutability === "payable" ? (
            <ActionParamFormGroup
              label={`${networkManager.symbol} to Send`}
              icon="fas fa-coins"
              value={this.state.amount}
              onChange={amount => this.setState({ amount })}
              placeholder="Default: 0"
            />
          ) : null}
          <Label>Constructor Parameters</Label>
          <ContractForm
            ref={this.form}
            key={selected}
            size="sm"
            {...constructorAbi}
            Empty={<div className="small">(None)</div>}
          />
          <div className="mb-2" />
        </>
      );
    }

    const needEstimate = this.needEstimate();

    return (
      <>
        <Button
          size="sm"
          color="default"
          id="toolbar-btn-deploy"
          key="toolbar-btn-deploy"
          className="rounded-0 border-0 flex-none px-2 w-5 flex-column align-items-center"
          onClick={this.onClick}
        >
          {icon}
        </Button>
        <UncontrolledTooltip
          trigger="hover"
          delay={0}
          placement="bottom"
          target="toolbar-btn-deploy"
        >
          {pending || "Deploy"}
        </UncontrolledTooltip>
        <Modal
          ref={this.modal}
          title={
            <span>
              Deploy Contract <b>{contractName}</b>
            </span>
          }
          textConfirm={needEstimate ? "Estimate & Deploy" : "Deploy"}
          pending={pending}
          onConfirm={this.confirmDeployment}
          textActions={this.renderTextActions()}
          onActions={[this.estimate]}
        >
          <DropdownInput
            label="Compiled contract (compiler output JSON)"
            options={contracts.map(c => ({
              id: c.path,
              display: c.relative || c.pathInProject,
            }))}
            placeholder="No Contract Selected"
            value={selected}
            onChange={this.updateContract}
          />
          {constructorParameters}
          <KeypairInputSelector
            label="Signer"
            extra={
              networkManager.browserExtension?.isEnabled &&
              signer && [
                {
                  group: networkManager.browserExtension.name.toLowerCase(),
                  badge: networkManager.browserExtension.name,
                  children: [
                    {
                      address: signer,
                      name: networkManager.browserExtension.name,
                    },
                  ],
                },
              ]
            }
            value={this.state.signer}
            onChange={signer => this.setState({ signer })}
          />
          <div className="row">
            {networkManager.sdk?.utils.txOptions?.list.map(option => (
              <ActionParamFormGroup
                key={`deploy-param-${option.name}`}
                className={option.className}
                label={option.label}
                icon={option.icon}
                value={this.state[option.name]}
                onChange={value => this.setState({ [option.name]: value })}
                placeholder={option.placeholder}
              />
            ))}
          </div>
        </Modal>
      </>
    );
  }
}
