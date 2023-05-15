import React from "react";
import ReactDOM from "react-dom";

import "./scss/index.scss";

import App from "./App";

import("./scss/fonts/montserrat/montserrat.css");
import("./scss/fonts/inter/inter.css");
import("./scss/fonts/hack/hack.css");
import("@fortawesome/fontawesome-free/js/all");

document.title = process.env.PROJECT_NAME;
ReactDOM.render(<App />, document.getElementById("root"));

window.addEventListener("auxclick", (event) => {
  if (event.button === 1) event.preventDefault();
});

window.addEventListener("contextmenu", (e) => {
  if (e.target.tagName !== "A") {
    e.preventDefault();
  }
});
