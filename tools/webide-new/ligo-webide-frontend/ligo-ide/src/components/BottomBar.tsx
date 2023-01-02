import { forwardRef, useImperativeHandle, useState } from "react";
import CacheRoute from "react-router-cache-route";

import { useSelector } from "react-redux";

import { actions, TerminalButton } from "~/base-components/workspace";
import ProtocolSelector from "~/ligo-components/eth-compiler/bottombar/ProtocolSelector";

// import { NetworkStatus } from '~/ligo-components/eth-network'
// import { QueueButton } from '~/ligo-components/eth-queue'
// import { CompilerSelectors } from "~/ligo-components/eth-compiler";

const BottomBar = forwardRef((_, ref) => {
  actions.bottomBarRef = ref;
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

  const [position, setPosition] = useState<[number, number]>([1, 1]);

  useImperativeHandle(ref, () => ({
    updatePosition(pos: [number, number]) {
      setPosition(pos);
    },
  }));

  let projectButtons;
  if (loaded) {
    projectButtons = (
      <>
        <CacheRoute
          path={["/local/:project"]}
          render={() => (
            <div
              className="p-1"
              style={{ fontSize: "14px" }}
            >{`Ln ${position[0]}, Col ${position[1]}`}</div>
          )}
        />
        <CacheRoute path={["/local/:project"]} component={ProtocolSelector} />
        <CacheRoute path={["/local/:project"]} component={TerminalButton} />
      </>
    );
  }

  return (
    <div className="border-top-1 d-flex flex-row">
      {/* { !noNetwork && <NetworkStatus /> }
      <QueueButton txs={txs} /> */}
      <div className="flex-1" />
      {projectButtons}
    </div>
  );
});

export default BottomBar;
