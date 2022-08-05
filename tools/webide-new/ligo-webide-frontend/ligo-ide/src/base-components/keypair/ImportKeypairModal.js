import React, { PureComponent } from "react";

import { Modal, DebouncedFormGroup, ButtonOptions, Label } from "~/base-components/ui-components";

import notification from "~/base-components/notification";

import keypairManager from "./keypairManager";

export default class ImportKeypairModal extends PureComponent {
  constructor(props) {
    super(props);
    const defaultChain = props.chains && props.chains[0].key;
    this.state = {
      chain: defaultChain,
      pending: false,
      name: "",
      secret: "",
      valid: false,
      feedback: "",
      keypair: null,
    };

    this.modal = React.createRef();
    this.input = React.createRef();
  }

  openModal(chain) {
    this.modal.current.openModal();
    if (chain) {
      this.setState({ chain });
    }
    this.setState({
      name: "",
      secret: "",
      keypair: null,
      valid: false,
      feedback: "",
    });
    setTimeout(() => this.input.current?.focus(), 100);
    return new Promise((resolve) => (this.onResolve = resolve));
  }

  onChange = (secret) => {
    this.setState({ secret });
    this.refreshKeypair(secret, this.state.chain);
  };

  setChain = (chain) => {
    const { secret } = this.state;
    this.setState({ chain });
    this.refreshKeypair(secret, chain);
  };

  refreshKeypair = (secret, chain) => {
    if (!secret) {
      this.setState({ keypair: null, valid: false, feedback: "" });
      return;
    }
    try {
      const keypair = this.props.kp.importKeypair(secret, chain);
      this.setState({
        keypair,
        valid: true,
        feedback: (
          <span>
            Address: <code>{keypair.address}</code>
          </span>
        ),
      });
    } catch (e) {
      console.warn(e);
      this.setState({
        keypair: null,
        valid: false,
        feedback: `Not a valid ${this.props.secretName.toLowerCase()}`,
      });
    }
  };

  onConfirm = async () => {
    const { name, keypair } = this.state;

    if (!keypair) {
      this.onResolve();
      return;
    }

    if (this.props.keypairs.find((k) => k.name === name)) {
      notification.error(
        "Import Keypair Failed",
        `The keypair name <b>${name}</b> has already been used.`
      );
      return;
    }

    if (this.props.keypairs.find((k) => k.address === keypair.address)) {
      notification.error(
        "Import Keypair Failed",
        `Keypair for <b>${keypair.address}</b> already exists.`
      );
      return;
    }

    this.setState({ pending: true });
    await keypairManager.saveKeypair(name, keypair);
    this.setState({ pending: false, secret: "" });

    this.modal.current.closeModal();
    this.onResolve(true);
  };

  renderChainOptions() {
    const { chains } = this.props;
    const { chain } = this.state;

    if (!chains) {
      return null;
    }
    return (
      <>
        <Label>The keypair can be used on</Label>
        <div>
          <ButtonOptions
            size="sm"
            className="mb-3"
            options={chains}
            selected={chain}
            onSelect={(chain) => this.setChain(chain)}
          />
        </div>
      </>
    );
  }

  render() {
    const { name, secret, valid, feedback } = this.state;

    return (
      <Modal
        ref={this.modal}
        title="Import Keypair"
        textConfirm="Import"
        pending={this.state.pending && "Importing..."}
        onConfirm={this.onConfirm}
        confirmDisabled={!name || !valid}
      >
        <DebouncedFormGroup
          ref={this.input}
          label="Name"
          maxLength="200"
          placeholder="Please enter a name for the keypair"
          onChange={(name) => this.setState({ name })}
        />
        {this.renderChainOptions()}
        <DebouncedFormGroup
          label={`Enter the ${this.props.secretName.toLowerCase()} you want to import`}
          maxLength="300"
          value={secret}
          onChange={this.onChange}
          feedback={feedback}
          valid={valid}
        />
      </Modal>
    );
  }
}
