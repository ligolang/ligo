import React, { useEffect, useState } from "react";
import { useLocation } from 'react-router-dom';
import styles from "./styles.module.css";
function SyntaxSwitch(props) {
  const [_, setState] = useState(0);
  const [isNext, setIsNext] = useState(false);
  const location = useLocation();

  // All this thing is a trick to force rerender
  // because it looks like there's a bug with static generation
  // so we have to trigger a render to make sure the correct value is rendered
  useEffect(() => {
    setState(1);
  }, []);
  useEffect(() => {
    let hidePascaligo = !!location.pathname?.startsWith("/docs/next");
    setIsNext(hidePascaligo);
  }, [location]);
  return /*#__PURE__*/React.createElement("select", {
    className: styles.syntaxSwitch,
    value: props.syntax,
    onChange: e => {
      if (typeof window === "undefined") return;
      const url = new URL(window.location);
      url.searchParams.set("lang", e.target.value);
      window.history.replaceState(null, "", url.toString());
      localStorage.setItem("syntax", e.target.value);
      props.onSyntaxChange(e.target.value);
    }
  }, /*#__PURE__*/React.createElement("option", {
    value: "cameligo"
  }, "CameLIGO"), /*#__PURE__*/React.createElement("option", {
    value: "jsligo"
  }, "JsLIGO"), !isNext && /*#__PURE__*/React.createElement("option", {
    value: "pascaligo"
  }, "PascaLIGO"));
}
export default SyntaxSwitch;