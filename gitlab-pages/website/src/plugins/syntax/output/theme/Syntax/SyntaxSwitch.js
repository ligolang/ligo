import { useHistory } from "@docusaurus/router";
import React, { useCallback } from "react";
import styles from "./styles.module.css";

function SyntaxSwitch(props) {
  const history = useHistory();
  const onSyntaxChange = useCallback(value => {
    if (typeof window === "undefined") return;
    history.replace({
      search: `?lang=${value}`
    });
    localStorage.setItem("syntax", value);
    props.onSyntaxChange(value);
  }, [props.syntax]);
  return /*#__PURE__*/React.createElement("form", null, /*#__PURE__*/React.createElement("div", {
    className: styles["switch__container"]
  }, /*#__PURE__*/React.createElement("label", {
    className: styles["switch__options-jsligo"],
    onClick: () => onSyntaxChange("jsligo")
  }, "JsLIGO"), /*#__PURE__*/React.createElement("button", {
    type: "button",
    role: "switch",
    className: styles.switch__button,
    "aria-label": `prefer ${props.syntax}`,
    "aria-checked": props.syntax === "cameligo",
    onClick: () => onSyntaxChange(props.syntax === "jsligo" ? "cameligo" : "jsligo")
  }, /*#__PURE__*/React.createElement("span", {
    className: styles["switch__button-circle"]
  })), /*#__PURE__*/React.createElement("label", {
    className: styles["switch__options-cameligo"],
    onClick: () => onSyntaxChange("cameligo")
  }, "CameLIGO")));
}

export default SyntaxSwitch;