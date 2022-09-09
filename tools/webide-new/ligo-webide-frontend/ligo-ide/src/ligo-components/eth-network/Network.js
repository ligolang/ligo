import React from "react";

import { connect } from "~/base-components/redux";

import networkManager from "./networkManager";
import LocalNetwork from "./LocalNetwork";
import CustomNetwork from "./CustomNetwork";
import RemoteNetwork from "./RemoteNetwork";
import { default as DefaultCustomNetworkModal } from "./CustomNetwork/CustomNetworkModal";

export default connect(["network", "customNetworks", "uiState"])((props) => {
  const {
    network: networkId = "dev",
    customNetworks,
    uiState,
    configButton,
    tabs,
    minerKey,
    CustomNetworkModal = DefaultCustomNetworkModal,
    cacheLifecycles,
  } = props;

  const [active, setActive] = React.useState(true);
  React.useEffect(() => {
    cacheLifecycles.didCache(() => setActive(false));
    cacheLifecycles.didRecover(() => setActive(true));
  });

  if (networkId === "dev") {
    return (
      <LocalNetwork
        networkId={networkId}
        active={active}
        configButton={configButton}
        tabs={tabs}
        minerKey={minerKey}
      />
    );
  }
  if (networkId.startsWith("custom")) {
    return (
      <CustomNetwork
        networkId={networkId}
        option={uiState.get("customNetworkOption")}
        customNetworks={customNetworks}
        CustomNetworkModal={CustomNetworkModal}
      />
    );
  }
  const url = networkManager.sdk?.url;
  return <RemoteNetwork networkId={networkId} url={url} />;
});
