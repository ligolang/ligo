import React from "react";

import { DebouncedInput } from "~/base-components/ui-components";

import IntegerParamInput from "./IntegerParamInput";
import StringParamInput from "./StringParamInput";

export default function ParamInput(props) {
  const { type, icon } = props;

  if (icon) {
    return (
      <DebouncedInput
        {...props}
        addon={
          <span key={icon.replace(/ /g, "-")}>
            <i className={icon} />
          </span>
        }
      />
    );
  }
  if (!type) {
    return "No type given";
  }
  if (type.startsWith("int")) {
    return <IntegerParamInput {...props} />;
  }
  if (type === "string") {
    return <StringParamInput {...props} />;
  }
  return `Unsupported type ${type}`;
}
