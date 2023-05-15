import { memo } from "react";
// eslint-disable-next-line @typescript-eslint/ban-ts-comment
// @ts-ignore
import Config from "Config";
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
    // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment, @typescript-eslint/no-unsafe-member-access, @typescript-eslint/no-unsafe-call
    const availablenetworks: { id: string; faucetUrl: string }[] = Config.networks
      .filter((v: { id: string; faucetUrl: string | undefined }) => !!v.faucetUrl)
      .map((v: { id: string; faucetUrl: string | undefined }) => {
        return { id: v.id, faucetUrl: v.faucetUrl };
      });

    const claim = () => {
      let faucetUrl;
      const isAvailable = availablenetworks.find((v) => v.id === network);
      if (isAvailable) {
        faucetUrl = isAvailable.faucetUrl;
      } else {
        return;
      }

      fileOps.openLink(faucetUrl);
    };

    return !availablenetworks.find((v) => v.id === network) ? null : isIconButton ? (
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
