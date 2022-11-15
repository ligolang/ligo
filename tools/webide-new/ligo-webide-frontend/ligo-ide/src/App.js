import React, { Suspense, lazy } from "react";
import { BrowserRouter, HashRouter, Switch, Route } from "react-router-dom";

import ReactGA from "react-ga4";
import platform from "~/base-components/platform";
import { LoadingScreen } from "~/base-components/ui-components";

if (process.env.MEASUREMENT_ID) {
  ReactGA.initialize(process.env.MEASUREMENT_ID);
}

const Router = platform.isDesktop ? HashRouter : BrowserRouter;
const ReduxApp = lazy(() => import("./ReduxApp.tsx" /* webpackChunkName: "components" */));

export default function App() {
  return (
    <Router>
      <Suspense fallback={<LoadingScreen />}>
        <Switch>
          <Route component={ReduxApp} />
        </Switch>
      </Suspense>
    </Router>
  );
}
