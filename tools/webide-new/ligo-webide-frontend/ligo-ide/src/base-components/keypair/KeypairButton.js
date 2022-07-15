import React, { PureComponent } from "react";
import { withRouter } from "react-router";
import redux from "~/base-components/redux";

import keypairManager from "./keypairManager";
import KeypairManagerModal from "./KeypairManagerModal";

class KeypairButton extends PureComponent {
  constructor(props) {
    super(props);
    this.modal = React.createRef();
  }

  componentDidMount() {
    keypairManager.loadAndUpdateKeypairs();
  }

  openModal = () => {
    let chain;
    if (this.props.chains) {
      const { network } = redux.getState();
      chain = this.props.chains.find(c => c.network === network || network.startsWith(c.key))?.key;
    }
    this.modal.current.openModal(chain);
  };

  render() {
    const {
      chains,
      mnemonic,
      secretName = "Private Key",
      modifyNameDisabled,
      deletionDisabled,
    } = this.props;

    return (
      <>
        <div onClick={this.openModal}>{this.props.children}</div>
        <KeypairManagerModal
          ref={this.modal}
          chains={chains}
          mnemonic={mnemonic}
          secretName={secretName}
          modifyNameDisabled={modifyNameDisabled}
          deletionDisabled={deletionDisabled}
        />
      </>
    );
  }
}

export default withRouter(KeypairButton);
