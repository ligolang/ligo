import React from "react";
import CacheRoute from "react-router-cache-route";

import { connect } from "~/base-components/redux";

import { KeypairButton } from "~/base-components/keypair";
import { TerminalButton } from "~/base-components/workspace";
import { UncontrolledTooltip } from "~/base-components/ui-components";
import ProtocolSelector from "~/ligo-components/eth-compiler/bottombar/ProtocolSelector";

// import { NetworkStatus } from '~/ligo-components/eth-network'
// import { QueueButton } from '~/ligo-components/eth-queue'
// import { CompilerSelectors } from "~/ligo-components/eth-compiler";

export default connect(["network", "queue", "projects", "uiState"])(function BottomBar(props) {
  const {
    network,
    queue,
    projects,
    uiState,

    mnemonic = true,
    secretName = mnemonic ? "Private Key / Mnemonic" : "Private Key",
    chains,

    // noNetwork,
  } = props;

  const localNetwork = uiState.get("localNetwork");
  let txs;
  if (network !== "dev") {
    txs = queue.getIn([network, "txs"]);
  } else if (localNetwork && localNetwork.lifecycle === "started") {
    txs = queue.getIn([localNetwork.params.id, "txs"]);
  }

  const selectedProject = projects.get("selected");
  const loaded = selectedProject?.get("loaded");
  let projectButtons;
  if (loaded) {
    projectButtons = (
      <>
        <CacheRoute path={["/local/:project"]} component={ProtocolSelector} />
        <CacheRoute path={["/local/:project"]} component={TerminalButton} />
      </>
    );
  }

  return (
    <>
      <KeypairButton mnemonic={mnemonic} secretName={secretName} chains={chains}>
        <div className="btn btn-primary btn-sm btn-flat" id="keypair-manager">
          <i className="fas fa-key" />
        </div>
        <UncontrolledTooltip placement="bottom" target="keypair-manager">
          Keypair Manager
        </UncontrolledTooltip>
      </KeypairButton>
      {/* { !noNetwork && <NetworkStatus /> }
    <QueueButton txs={txs} /> */}
      <div className="flex-1" />
      {projectButtons}
    </>
  );
});
