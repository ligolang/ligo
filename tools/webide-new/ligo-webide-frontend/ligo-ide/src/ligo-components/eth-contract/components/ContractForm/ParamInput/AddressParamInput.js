import React from "react";

import { networkManager } from "~/ligo-components/eth-network";
import { KeypairInputSelector } from "~/base-components/keypair";

export default function AddressParamInput({ size, value, onChange, disabled, maxLength = 128 }) {
  const onChangeValue = (value = "") => {
    onChange(value, { raw: value, display: value, empty: !value });
  };

  React.useEffect(() => {
    onChangeValue(value);
  }, []);

  return (
    <KeypairInputSelector
      size={size}
      editable
      maxLength={maxLength}
      icon="fas fa-map-marker-alt"
      extra={
        networkManager.browserExtension?.isEnabled && [
          {
            group: networkManager.browserExtension.name.toLowerCase(),
            badge: networkManager.browserExtension.name,
            children:
              networkManager.browserExtension?.allAccounts?.map(address => ({
                address,
              })) || [],
          },
        ]
      }
      value={value}
      onChange={onChangeValue}
      disabled={disabled}
    />
  );
}
