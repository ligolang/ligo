import React, { PureComponent } from "react";
import classnames from "classnames";

import {
  Screen,
  ButtonGroup,
  UncontrolledButtonDropdown,
  ToolbarButton,
  DropdownToggle,
  DropdownMenu,
  DropdownItem,
  Badge,
} from "~/base-components/ui-components";

import { KeypairInputSelector } from "~/base-components/keypair";
import { networkManager } from "~/ligo-components/eth-network";

import FormSection from "./FormSection";
import MarginlessFormSection from "./MarginlessFormSection";

import ContractForm from "./ContractForm";
import ActionParamFormGroup from "./ContractForm/ActionParamFormGroup";

export default class AbiActionForm extends PureComponent {
  static FormSection = FormSection;

  static MarginlessFormSection = MarginlessFormSection;

  static defaultProps = {
    FormSection: MarginlessFormSection,
  };

  constructor(props) {
    super(props);

    const selected = Math.max(
      props.actions.findIndex(item => !item.header && !item.divider),
      0
    );
    this.state = {
      selected,
      amount: "",
      signer: "",
      executing: false,
      actionError: "",
      actionResult: "",
      format: "pretty",
    };
    this.form = React.createRef();
  }

  get selectedAction() {
    return this.props.actions[this.state.selected] || {};
  }

  selectAction = index => {
    this.setState({
      selected: index,
      amount: "",
      executing: false,
      actionError: "",
      actionResult: "",
    });
  };

  estimate = async actionName => {};

  executeAction = async actionName => {
    await this.props.executeAction(actionName, this);
  };

  renderActionSelector = () => {
    const { selectedAction } = this;
    const {
      inModal,
      smDropdown,
      selectorHeader = "actions",
      selectorIcon = "fas fa-calculator",
      actions,
    } = this.props;
    return (
      <ButtonGroup>
        <UncontrolledButtonDropdown size="sm">
          <DropdownToggle
            color="primary"
            caret
            className={classnames(!inModal && "rounded-0 border-0")}
          >
            <i className={selectorIcon} />
            <code className="ml-2 mr-1">
              <b>{selectedAction.name}</b>
            </code>
          </DropdownToggle>
          <DropdownMenu className={classnames(smDropdown && "dropdown-menu-sm")}>
            {selectorHeader && <DropdownItem header>{selectorHeader}</DropdownItem>}
            {actions.map((item, index) => {
              if (item.header) {
                return (
                  <DropdownItem key={item.header} header>
                    {item.header}
                  </DropdownItem>
                );
              }
              if (item.divider) {
                return <DropdownItem key={`divider-${index}`} divider />;
              }
              return (
                <DropdownItem
                  key={item.name}
                  className={classnames({
                    active: index === this.state.selected,
                  })}
                  onClick={() => this.selectAction(index)}
                >
                  <code>{item.name}</code>
                </DropdownItem>
              );
            })}
          </DropdownMenu>
        </UncontrolledButtonDropdown>
        <ToolbarButton
          id={this.props.toolbarId}
          rounded={inModal}
          className={!inModal && "border-right-1"}
          color={inModal ? "primary" : "default"}
          key={this.state.executing ? "action-executing" : "action-execute"}
          icon={this.state.executing ? "fas fa-spin fa-spinner" : "fas fa-play"}
          tooltip="Execute"
          onClick={() => this.executeAction(selectedAction.name)}
        />
      </ButtonGroup>
    );
  };

  renderGasOptions = selectedAction => {
    const { noGasOptions, FormSection } = this.props;
    if (noGasOptions) {
      return null;
    }
    const txOptions = networkManager.sdk?.utils.txOptions;
    if (!txOptions?.list.length) {
      return null;
    }
    const estimate = (
      <Badge
        color="primary"
        onClick={evt => {
          evt.stopPropagation();
          this.estimate(selectedAction.name);
        }}
      >
        Estimate
      </Badge>
    );
    return (
      <FormSection title={txOptions.title} right={estimate}>
        {txOptions.list.map(option => (
          <ActionParamFormGroup
            size="sm"
            key={`param-${option.name}`}
            label={option.label}
            icon={option.icon}
            value={this.state[option.name]}
            onChange={value => this.setState({ [option.name]: value })}
            placeholder={option.placeholder}
          />
        ))}
      </FormSection>
    );
  };

  renderAuthorization = () => {
    if (!this.props.signerSelector) {
      return null;
    }

    const { FormSection, signer } = this.props;
    return (
      <FormSection title="Authorization">
        <KeypairInputSelector
          size="sm"
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
      </FormSection>
    );
  };

  renderResult = () => {
    const { FormSection, showResult } = this.props;
    if (!showResult) {
      return null;
    }

    let title = "Result";
    let badge = null;
    if (this.state.actionResult) {
      title = (
        <span key="success">
          Result
          <i className="fas fa-check-circle text-success ml-1" />
        </span>
      );
      badge = this.renderResultBadge();
    } else if (this.state.actionError) {
      title = (
        <span key="success">
          Result
          <i className="fas fa-exclamation-triangle text-danger ml-1" />
        </span>
      );
    }

    return (
      <FormSection title={title} right={badge}>
        {this.renderResultContent()}
      </FormSection>
    );
  };

  renderResultBadge = () => {
    if (this.props.noResultBadge) {
      return null;
    }
    const { format } = this.state;
    return (
      <div className="badge-group">
        <Badge
          className={format === "pretty" ? "bg-primary" : "bg-hover"}
          onClick={() => this.setState({ format: "pretty" })}
        >
          Pretty
        </Badge>
        <Badge
          className={format === "raw" ? "bg-primary" : "bg-hover"}
          onClick={() => this.setState({ format: "raw" })}
        >
          Raw
        </Badge>
      </div>
    );
  };

  renderResultContent = () => {
    const { actionError, actionResult } = this.state;
    if (actionError) {
      return (
        <div>
          <span>{actionError}</span>
        </div>
      );
    }

    if (actionResult) {
      return <pre className="text-body pre-wrap break-all small user-select">{actionResult}</pre>;
    }

    return <div className="small">(None)</div>;
  };

  render() {
    const { FormSection, inModal, actions } = this.props;

    if (!actions.length) {
      return (
        <Screen>
          <p>No actions found</p>
        </Screen>
      );
    }

    const { selectedAction } = this;
    return (
      <div className="d-flex flex-column align-items-stretch h-100">
        <div className={classnames("d-flex", inModal ? "mb-3" : "border-bottom-1")}>
          {this.renderActionSelector()}
        </div>
        <div className={classnames(!inModal && "d-flex flex-column flex-grow-1 overflow-auto")}>
          <FormSection title="Parameters">
            {selectedAction.payable || selectedAction.stateMutability === "payable" ? (
              <ActionParamFormGroup
                size="sm"
                label={`${networkManager.symbol} to Send`}
                icon="fas fa-coins"
                value={this.state.amount}
                onChange={amount => this.setState({ amount })}
                placeholder="Default: 0"
              />
            ) : null}
            <ContractForm
              ref={this.form}
              key={selectedAction.name}
              size="sm"
              {...selectedAction}
              Empty={<div className="small">(None)</div>}
            />
          </FormSection>
          {this.renderGasOptions(selectedAction)}
          {this.renderAuthorization()}
          {this.renderResult()}
        </div>
      </div>
    );
  }
}
