import React from "react";
import { Badge } from "~/base-components/ui-components";
import { networkManager } from "~/ligo-components/eth-network";

export default function (props) {
  return <Badge pill>{networkManager.sdk?.utils.display(props.value)}</Badge>;
}
