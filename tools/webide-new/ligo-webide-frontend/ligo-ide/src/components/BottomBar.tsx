import React from "react";
import CacheRoute from "react-router-cache-route";

import { useSelector } from "react-redux";

import { KeypairButton } from "~/base-components/keypair";
import { TerminalButton } from "~/base-components/workspace";
import { UncontrolledTooltip } from "~/base-components/ui-components";
import ProtocolSelector from "~/ligo-components/eth-compiler/bottombar/ProtocolSelector";

// import { NetworkStatus } from '~/ligo-components/eth-network'
// import { QueueButton } from '~/ligo-components/eth-queue'
// import { CompilerSelectors } from "~/ligo-components/eth-compiler";

type BottomBarProps = {
  mnemonic?: boolean;
  secretName?: string;
  chains?: any[];
};

const BottomBar: React.FC<BottomBarProps> = ({
  mnemonic = true,
  secretName = mnemonic ? "Private Key / Mnemonic" : "Private Key",
  chains,
}) => {
  /* eslint-disable */
  // @ts-ignore
  const network: string = useSelector((state) => state.network);
  // const queue = useSelector((state) => state.queue);
  // @ts-ignore
  const projects = useSelector((state) => state.projects);
  // const uiState = useSelector((state) => state.uiState);

  // const localNetwork = uiState.get("localNetwork");
  // let txs;
  // if (network !== "dev") {
  //   txs = queue.getIn([network, "txs"]);
  // } else if (localNetwork && localNetwork.lifecycle === "started") {
  //   txs = queue.getIn([localNetwork.params.id, "txs"]);
  // }
  const selectedProject = projects.get("selected");
  const loaded = selectedProject?.get("loaded");
  /* eslint-enable */

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
    <div className="border-top-1 d-flex flex-row">
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
    </div>
  );
};

export default BottomBar;
