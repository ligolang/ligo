import React, { useEffect, useState } from "react";
import { withRouter, useParams } from "react-router-dom";
import redux, { connect } from "~/base-components/redux";
import networkManager from "./networkManager";
import RemoteNetwork from "./RemoteNetwork";
import { default as DefaultCustomNetworkModal } from "./CustomNetwork/CustomNetworkModal";

export default connect(["network", "customNetworkModalStatus", "networkConnect"])(
  withRouter((props) => {
    const {
      network: networkId = "dev",
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
      if (cacheLifecycles) {
        cacheLifecycles.didCache(() => setActive(false));
        cacheLifecycles.didRecover(() => setActive(true));
      }
    });

    return <RemoteNetwork networkId={networkId} url={getRpcUrl()} />;
  })
);
