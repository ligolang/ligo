import React from "react";
import classnames from "classnames";

import { Button, ButtonGroup } from "reactstrap";

export default function ButtonOptions({ size, options, selected, className = "mb-2", onSelect }) {
  return (
    <ButtonGroup size={size} className={className}>
      {options.map(({ key, icon, text }) => (
        <Button
          key={`option-${key}`}
          color={selected === key ? "primary" : "secondary"}
          onClick={() => onSelect(key)}
        >
          {icon ? <i className={classnames(icon, "mr-1")} /> : null}
          {text}
        </Button>
      ))}
    </ButtonGroup>
  );
}
