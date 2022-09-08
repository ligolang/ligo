import React, { PureComponent } from "react";

import { Modal, DebouncedFormGroup } from "~/base-components/ui-components";

import { KeypairInputSelector } from "~/base-components/keypair";
import notification from "~/base-components/notification";

import { networkManager } from "~/ligo-components/eth-network";
import { ActionParamFormGroup } from "~/ligo-components/eth-contract";

export default class SignRequestModal extends PureComponent {
  constructor(props) {
    super(props);
    this.state = {
      from: "",
      to: "",
      data: "",
    };
    this.modal = React.createRef();
  }

  // eslint-disable-next-line react/no-unused-class-component-methods
  openModal = async (tx) => {
    // eslint-disable-next-line react/no-unused-class-component-methods
    this.tx = tx;
    const state = {
      from: networkManager.sdk?.utils.formatAddress(tx.from, networkManager.sdk?.chainId),
      to: tx.to,
    };
    networkManager.sdk?.utils.txOptions?.list.forEach((option) => {
      if (tx[option.name]) {
        state[option.name] = BigInt(tx[option.name]).toString();
      } else if (tx[option.alias]) {
        state[option.name] = BigInt(tx[option.alias]).toString();
      }
    });
    state.value = networkManager.sdk.utils.unit.fromValue(tx.value || 0);
    state.data = tx.data;

    this.setState(state);
    this.modal.current.openModal();
    return new Promise((resolve, reject) => {
      this.onResolve = resolve;
      this.onReject = reject;
    });
  };

  onConfirm = async () => {
    const tx = {};
    const { from, to, value, data, gasLimit, gasPrice } = this.state;
    tx.from = from;
    if (to) {
      tx.to = to;
    }
    if (data) {
      tx.data = data;
    }
    try {
      tx.value = `0x${BigInt(networkManager.sdk.utils.unit.toValue(value || 0)).toString(16)}`;
    } catch {
      notification.error(
        `Invalid ${networkManager.symbol} to Send`,
        "Please enter a valid number."
      );
      return;
    }
    try {
      tx.gasLimit = `0x${BigInt(gasLimit || 0).toString(16)}`;
    } catch {
      notification.error("Invalid Gas Limit", "Please enter a valid number.");
      return;
    }
    try {
      tx.gasPrice = `0x${BigInt(gasPrice || 0).toString(16)}`;
    } catch {
      notification.error("Invalid Gas Price", "Please enter a valid number.");
      return;
    }
    this.onResolve(tx);
    this.modal.current.closeModal();
  };

  onClosed = () => this.onReject();

  render() {
    const { from, to, data = "" } = this.state;
    const prefix = data.substr(0, 34);
    return (
      <Modal
        ref={this.modal}
        title="Sign Request"
        textConfirm="Sign & Send Transaction"
        onConfirm={this.onConfirm}
        onClosed={this.onClosed}
      >
        <KeypairInputSelector
          label="From"
          value={from}
          onChange={(from) => this.setState({ from })}
        />
        {to ? (
          <KeypairInputSelector
            label="To"
            editable
            value={to}
            onChange={(to) => this.setState({ to })}
          />
        ) : (
          <DebouncedFormGroup label="To" value="Contract Creation" disabled />
        )}
        {data && data !== "0x" && (
          <DebouncedFormGroup
            label="Data"
            value={`${prefix}... (${data.length / 2 - 1} Bytes)`}
            disabled
          />
        )}
        <ActionParamFormGroup
          label={`${networkManager.symbol} to Send`}
          icon="fas fa-coins"
          value={this.state.value}
          onChange={(value) => this.setState({ value })}
          disabled
          placeholder="Default: 0"
        />
        <div className="row">
          {networkManager.sdk?.utils.txOptions?.list.map((option) => (
            <ActionParamFormGroup
              key={`deploy-param-${option.name}`}
              className={option.className}
              label={option.label}
              icon={option.icon}
              value={this.state[option.name]}
              onChange={(value) => this.setState({ [option.name]: value })}
              disabled
              placeholder={option.placeholder}
            />
          ))}
        </div>
      </Modal>
    );
  }
}
