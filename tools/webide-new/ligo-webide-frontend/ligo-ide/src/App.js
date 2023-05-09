import React, { Suspense, lazy } from "react";
import { BrowserRouter, Switch, Route } from "react-router-dom";
import * as onigasm from "onigasm/lib/onigasm.wasm";
import { loadWASM } from "onigasm";
import ReactGA from "react-ga4";

import { LoadingScreen } from "~/base-components/ui-components";

if (process.env.MEASUREMENT_ID) {
  ReactGA.initialize(process.env.MEASUREMENT_ID);
}

const ReduxApp = lazy(async () => {
  await loadWASM(onigasm.default).catch((e) => {
    console.error(e);
  });
  return import("./ReduxApp.tsx" /* webpackChunkName: "components" */);
});

export default function App() {
  return (
    <BrowserRouter>
      <Suspense fallback={<LoadingScreen />}>
        <Switch>
          <Route component={ReduxApp} />
        </Switch>
      </Suspense>
    </BrowserRouter>
  );
}
