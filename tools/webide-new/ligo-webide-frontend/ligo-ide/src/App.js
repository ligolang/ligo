import React, { Suspense, lazy } from "react";
import { BrowserRouter, HashRouter, Switch, Route } from "react-router-dom";

import platform from "~/base-components/platform";
import { LoadingScreen } from "~/base-components/ui-components";

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
