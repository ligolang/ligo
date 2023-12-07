import { useHistory, useLocation } from "@docusaurus/router";
import React, { useCallback, useEffect, useState } from "react";
import styles from "./styles.module.css";

function SyntaxSwitch(props) {
  const [isNext, setIsNext] = useState(false);
  const location = useLocation();
  const history = useHistory();
  useEffect(() => {
    let hidePascaligo = !location.pathname?.startsWith("/docs/0.72.0") && !location.pathname?.startsWith("/docs/0.73.0");
    setIsNext(hidePascaligo);
  }, [location]);
  const onSyntaxChange = useCallback(value => {
    if (typeof window === "undefined") return;
    history.replace({
      search: `?lang=${value}`
    });
    localStorage.setItem("syntax", value);
    props.onSyntaxChange(value);
  }, [props.syntax]);
  return isNext ? /*#__PURE__*/React.createElement("form", null, /*#__PURE__*/React.createElement("div", {
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
  }, "CameLIGO"))) : /*#__PURE__*/React.createElement("select", {
    className: styles.syntaxSwitch,
    value: props.syntax,
    onChange: e => onSyntaxChange(e.target.value)
  }, /*#__PURE__*/React.createElement("option", {
    value: "cameligo"
  }, "CameLIGO"), /*#__PURE__*/React.createElement("option", {
    value: "jsligo"
  }, "JsLIGO"), /*#__PURE__*/React.createElement("option", {
    value: "pascaligo"
  }, "PascaLIGO"));
}

export default SyntaxSwitch;