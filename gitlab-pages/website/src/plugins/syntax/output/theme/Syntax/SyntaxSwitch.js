import React from 'react';
import styles from './styles.module.css';
function SyntaxSwitch(props) {
  return /*#__PURE__*/React.createElement("select", {
    className: styles.syntaxSwitch,
    defaultValue: props.syntax,
    onChange: e => {
      if (typeof window === 'undefined') return;
      const url = new URL(window.location);
      url.searchParams.set("lang", e.target.value);
      window.history.pushState(null, "", url.toString());
      props.onSyntaxChange(e.target.value);
    }
  }, /*#__PURE__*/React.createElement("option", {
    value: "pascaligo"
  }, "PascaLIGO"), /*#__PURE__*/React.createElement("option", {
    value: "cameligo"
  }, "CameLIGO"), /*#__PURE__*/React.createElement("option", {
    value: "reasonligo"
  }, "ReasonLIGO"), /*#__PURE__*/React.createElement("option", {
    value: "jsligo"
  }, "JsLIGO"));
}
export default SyntaxSwitch;