import React from "react";

import ActionParamFormGroup from "../ActionParamFormGroup";

export default function TupleInput({ size, label, value, onChange, components, disabled }) {
  const valueArray = components.map((_, index) => (value && value[index]) || "");

  const tupleError = React.useMemo(() => new Array(components.length).fill(undefined), []);
  const tupleRaw = React.useMemo(() => new Array(components.length).fill(""), []);
  const tupleEmpty = React.useMemo(() => new Array(components.length).fill(true), []);
  const tupleDisplay = React.useMemo(() => {
    const obj = {};
    components.forEach(({ name, type }, index) => {
      const key = name || `(param${index})`;
      obj[key] = { type, value: "" };
    });
    return obj;
  }, []);

  const onChangeValue = (subvalue, { error, raw, display, empty }, { name, index }) => {
    valueArray[index] = subvalue;
    tupleError[index] = error;
    tupleRaw[index] = raw;
    tupleEmpty[index] = empty;
    const key = name || `(param${index})`;
    tupleDisplay[key].value = display;
    onChange(valueArray, {
      error: tupleError.find(Boolean),
      raw: tupleRaw,
      display: tupleDisplay,
      empty: tupleEmpty.every(Boolean),
    });
  };

  return (
    <div className="pl-1">
      {components.map(({ name, type, components }, index) => (
        <ActionParamFormGroup
          key={`struct-${label}-${index}`}
          size={size}
          label={name ? `${label}.${name}` : `${label}[${index}]`}
          type={type}
          components={components}
          value={valueArray[index]}
          onChange={(subvalue, extra) => onChangeValue(subvalue, extra, { name, index })}
          disabled={disabled}
        />
      ))}
    </div>
  );
}
