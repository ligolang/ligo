import React from "react";

import CenterScreen from "./CenterScreen";

export default function (props) {
  const { text = "Loading" } = props;
  return (
    <CenterScreen>
      <i className="fas fa-spin fa-spinner mr-2" />
      {text}
    </CenterScreen>
  );
}
