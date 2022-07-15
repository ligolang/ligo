import React from "react";
import ReactDOM from "react-dom";

import "./scss/index.scss";

import App from "./App";

if (!process.env.CDN) {
  import("./scss/fonts/montserrat/montserrat.css");
  import("./scss/fonts/hack/hack.css");
  import("@fortawesome/fontawesome-free/js/all");
}

document.title = process.env.PROJECT_NAME;
ReactDOM.render(<App />, document.getElementById("root"));

window.addEventListener("auxclick", event => {
  if (event.button === 1) event.preventDefault();
});

window.addEventListener("contextmenu", e => e.preventDefault());
