import { memo } from "react";
import { ToolbarButton, IconButton } from "~/base-components/ui-components";

import fileOps from "~/base-components/file-ops";

type FaucetButtonProps = {
  network: string;
  isIconButton: boolean;
  kid: string;
  children: React.ReactNode;
};

const FaucetButton: React.FC<FaucetButtonProps> = memo(
  ({ network, isIconButton, kid, children }) => {
    const availablenetworks = ["tezostest", "tezostestlima", "t4l3nttest", "tezostestgostnet"];

    const claim = () => {
      let faucetUrl;
      if (network === "tezostest") {
        faucetUrl = "https://faucet.kathmandunet.teztnets.xyz/";
      } else if (network === "t4l3nttest") {
        faucetUrl = "https://github.com/Decentralized-Pictures/T4L3NT";
      } else if (network === "tezostestgostnet") {
        faucetUrl = "https://faucet.ghostnet.teztnets.xyz/";
      } else if (network === "tezostestlima") {
        faucetUrl = "https://faucet.limanet.teztnets.xyz/";
      } else {
        return;
      }

      fileOps.openLink(faucetUrl);
    };

    return availablenetworks.indexOf(network) === -1 ? null : isIconButton ? (
      <IconButton
        color="transparent"
        id={`kp-faucet-${kid}`}
        className="text-muted hover-show"
        icon="fas fa-faucet"
        onClick={claim}
      >
        {children}
      </IconButton>
    ) : (
      <ToolbarButton id="navbar-faucet" size="md" icon="fas fa-faucet" onClick={claim} />
    );
  }
);

export default FaucetButton;
