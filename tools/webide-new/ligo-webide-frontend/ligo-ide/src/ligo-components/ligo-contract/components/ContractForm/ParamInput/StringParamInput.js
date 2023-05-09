import React from "react";

import { Badge, DebouncedInput } from "~/base-components/ui-components";

export default function StringParamInput({ size, value, onChange, placeholder, disabled }) {
  const onChangeValue = (value) => {
    onChange(value, { display: value, raw: value, empty: !value });
  };

  React.useEffect(() => {
    onChangeValue(value);
  }, []);

  return (
    <div style={{ position: "relative" }}>
      <DebouncedInput
        type="textarea"
        size={size}
        value={value}
        onChange={onChangeValue}
        placeholder={placeholder}
        disabled={disabled}
      />
      <Badge
        style={{
          position: "absolute",
          right: "5px",
          bottom: "5px",
          height: "18px",
          zIndex: 100,
        }}
      >
        UTF8
      </Badge>
    </div>
  );
}
