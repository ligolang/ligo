import React, { memo, useEffect, useRef } from "react";
import { useSelector } from "react-redux";

import KeypairManagerModal from "./KeypairManagerModal";

type KeypairButtonProps = {
  chains?: any[];
  mnemonic: boolean;
  secretName: string;
  modifyNameDisabled?: string;
  deletionDisabled?: string;
  children?: React.ReactNode;
  isOpenKeypair: boolean;
  onCancel: () => void;
};

const KeypairButton: React.FC<KeypairButtonProps> = memo(
  ({
    chains,
    mnemonic = true,
    secretName = "Private Key",
    modifyNameDisabled,
    deletionDisabled,
    children,
    isOpenKeypair,
    onCancel,
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

    useEffect(() => {
      if (isOpenKeypair && modal.current) {
        openModal();
      }
      // eslint-disable-next-line react-hooks/exhaustive-deps
    }, [isOpenKeypair]);

    return (
      <>
        <div className="nav-link-inner" onClick={openModal}>
          {children}
        </div>
        <KeypairManagerModal
          ref={modal}
          chains={chains}
          mnemonic={mnemonic}
          secretName={secretName}
          modifyNameDisabled={modifyNameDisabled}
          deletionDisabled={deletionDisabled}
          onCancel={onCancel}
        />
      </>
    );
  }
);

export default KeypairButton;
