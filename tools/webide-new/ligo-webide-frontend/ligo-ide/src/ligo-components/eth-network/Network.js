import React, { useEffect, useState, useRef } from "react";
import { withRouter, useParams } from "react-router-dom";
import redux, { connect } from "~/base-components/redux";
import networkManager from "./networkManager";
import LocalNetwork from "./LocalNetwork";
import RemoteNetwork from "./RemoteNetwork";
import { default as DefaultCustomNetworkModal } from "./CustomNetwork/CustomNetworkModal";

export default connect([
  "network",
  // "customNetworks",
  // "uiState",
  "customNetworkModalStatus",
  "networkConnect",
])(
  withRouter((props) => {
    const {
      network: networkId = "dev",
      // customNetworks,
      // uiState,
      configButton,
      tabs,
      minerKey,
      CustomNetworkModal = DefaultCustomNetworkModal,
      cacheLifecycles,
      history,
      customNetworkModalStatus,
      networkConnect,
    } = props;

    const [active, setActive] = useState(true);
    // const [showCustomNetworkModal, setShowCustomNetworkModal] = useState(false);
    // const customModal = useRef();
    const paramsNetworkValue = useParams()?.network;

    const getRpcUrl = () => {
      const { isWallet, current } = networkManager;
      return isWallet ? current?.url || "" : networkManager.sdk?.url || "";
    };

    const updateNetworkPage = (id) => {
      const matchedNet = networkManager.findChainById(id);
      if (!matchedNet || id === "custom") return;
      networkManager.setNetwork(matchedNet);
    };

    useEffect(() => {
      history.location.pathname?.startsWith("/network") &&
        redux.dispatch("LOAD_NETWORK_RESOURCES", true);
      // if (customNetworkModalStatus) {
      // setShowCustomNetworkModal(true);
      // customModal.current?.openModal();
      // }
    }, [customNetworkModalStatus]);

    useEffect(() => {
      if (cacheLifecycles) {
        cacheLifecycles.didCache(() => setActive(false));
        cacheLifecycles.didRecover(() => setActive(true));
      }
    });

    // useEffect(() => {
    //   if (!networkId || (networkManager.network && networkId === networkManager.network.id)) return;
    //   updateNetworkPage(networkId);
    // }, [networkId, networkManager.networks]);

    // function customNetworkModalBody() {
    //   return (
    //     <CustomNetworkModal
    //       ref={customModal}
    //       networkId={networkId}
    //       customNetworks={customNetworks}
    //       option={uiState.get("customNetworkOption")}
    //       openModal={showCustomNetworkModal}
    //     />
    //   );
    // }

    if (networkId === "dev") {
      return (
        <>
          <LocalNetwork
            networkId={networkId}
            active={active}
            configButton={configButton}
            tabs={tabs}
            minerKey={minerKey}
          />
          {/* {customNetworkModalBody()} */}
        </>
      );
    }
    return (
      <>
        <RemoteNetwork networkId={networkId} url={getRpcUrl()} />
        {/* {customNetworkModalBody()} */}
      </>
    );
  })
);
