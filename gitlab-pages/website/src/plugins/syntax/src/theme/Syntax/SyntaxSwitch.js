import { useHistory } from "@docusaurus/router";
import React, { useCallback } from "react";

import styles from "./styles.module.css";

function SyntaxSwitch(props) {
  const history = useHistory();

  const onSyntaxChange = useCallback(
    (value) => {
      if (typeof window === "undefined") return;
      history.replace({
        search: `?lang=${value}`,
      });
      localStorage.setItem("syntax", value);
      props.onSyntaxChange(value);
    },
    [props.syntax]
  );

  return (
    <form>
      <div className={styles["switch__container"]}>
        <label
          className={styles["switch__options-jsligo"]}
          onClick={() => onSyntaxChange("jsligo")}
        >
          JsLIGO
        </label>
        <button
          type="button"
          role="switch"
          className={styles.switch__button}
          aria-label={`prefer ${props.syntax}`}
          aria-checked={props.syntax === "cameligo"}
          onClick={() => onSyntaxChange(props.syntax === "jsligo" ? "cameligo" : "jsligo")}
        >
          <span className={styles["switch__button-circle"]}></span>
        </button>
        <label
          className={styles["switch__options-cameligo"]}
          onClick={() => onSyntaxChange("cameligo")}
        >
          CameLIGO
        </label>
      </div>
    </form>
  );
}

export default SyntaxSwitch;
