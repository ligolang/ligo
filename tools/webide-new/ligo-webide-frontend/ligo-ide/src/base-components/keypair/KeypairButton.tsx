import React, { memo, useRef } from "react";
import { useSelector } from "react-redux";

import KeypairManagerModal from "./KeypairManagerModal";

type KeypairButtonProps = {
  chains?: any[];
  mnemonic: boolean;
  secretName: string;
  modifyNameDisabled?: string;
  deletionDisabled?: string;
  children?: React.ReactNode;
};

const KeypairButton: React.FC<KeypairButtonProps> = memo(
  ({
    chains,
    mnemonic,
    secretName = "Private Key",
    modifyNameDisabled,
    deletionDisabled,
    children,
  }) => {
    const modal = useRef<KeypairManagerModal>(null);
    /* eslint-disable */
    // @ts-ignore
    const network: string = useSelector((state) => state.network);
    /* eslint-enable */

    const openModal = () => {
      let chain;
      if (chains) {
        // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment, @typescript-eslint/no-unsafe-member-access, @typescript-eslint/no-unsafe-argument
        chain = chains.find((c) => c.network === network || network.startsWith(c.key))?.key;
      }
      if (modal.current) {
        modal.current.openModal(chain);
      }
    };

    return (
      <>
        <div onClick={openModal}>{children}</div>
        <KeypairManagerModal
          ref={modal}
          chains={chains}
          mnemonic={mnemonic}
          secretName={secretName}
          modifyNameDisabled={modifyNameDisabled}
          deletionDisabled={deletionDisabled}
        />
      </>
    );
  }
);

export default KeypairButton;
