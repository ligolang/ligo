import React from "react";

import { DebouncedInput } from "~/base-components/ui-components";

import ArrayParamInput from "./ArrayParamInput";
import TupleInput from "./TupleInput";
import BoolParamInput from "./BoolParamInput";
import IntegerParamInput from "./IntegerParamInput";
import StringParamInput from "./StringParamInput";
import BytesParamInput from "./BytesParamInput";
import AddressParamInput from "./AddressParamInput";

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
  if (type.endsWith("]")) {
    return <ArrayParamInput {...props} />;
  }
  if (type === "tuple") {
    return <TupleInput {...props} />;
  }
  if (type === "bool") {
    return <BoolParamInput {...props} />;
  }
  if (type.startsWith("int") || type.startsWith("uint")) {
    return <IntegerParamInput {...props} />;
  }
  if (type === "string") {
    return <StringParamInput {...props} />;
  }
  if (type === "byte" || type.startsWith("bytes")) {
    return <BytesParamInput {...props} />;
  }
  if (type === "address" || type === "FixedHash<20>") {
    return <AddressParamInput {...props} />;
  }
  return `Unsupported type ${type}`;
}
