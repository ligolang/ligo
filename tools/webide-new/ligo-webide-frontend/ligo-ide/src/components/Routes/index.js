import React, { Suspense, lazy } from "react";
import { Route, Redirect } from "react-router-dom";
import CacheRoute, { CacheSwitch } from "react-router-cache-route";

import { Input, LoadingScreen, CenterScreen } from "~/base-components/ui-components";

Input.defaultProps = {
  type: "text",
  autoComplete: "off",
  autoCorrect: "off",
  autoCapitalize: "off",
  spellCheck: "false",
};

const UserHomepage = lazy(() => import("./UserHomepage" /* webpackChunkName: "Homepage" */));
const Project = lazy(() => import("./Project" /* webpackChunkName: "Project" */));
const Network = lazy(() => import("./Network" /* webpackChunkName: "Network" */));
const OpenProject = lazy(() => import("./OpenProject" /* webpackChunkName: "OpenProject" */));

export default function (props) {
  return (
    <>
      {props.children}
      <Suspense fallback={<LoadingScreen />}>
        <CacheSwitch>
          <Route exact path="/" render={() => <Redirect to="/local" />} />
          <CacheRoute
            exact
            path="/network/:network?"
            component={Network}
            className="p-relative w-100 h-100"
          />
          <CacheRoute
            exact
            path="/:username"
            className="p-relative w-100 h-100 customHeight"
            component={UserHomepage}
          />
          <CacheRoute
            exact
            path="/share/:gistid"
            className="p-relative w-100 h-100"
            render={(routerProps) => {
              const projectLink = routerProps.match?.params?.gistid;
              if (!projectLink || !projectLink || projectLink === "local") {
                const isMatch = projectLink.match(/^[0-9,a-f]{32}$/gi);
                if (isMatch === null || isMatch.length === 0) {
                  return <Redirect to="/local}" />;
                }
              }
              return <OpenProject gistId={projectLink} />;
            }}
          />
          <CacheRoute
            exact
            path="/:username/:project"
            cacheKey="project-editor"
            component={Project}
            className="p-relative w-100 h-100"
          />
          <Route render={() => <CenterScreen>Invalid URL</CenterScreen>} />
        </CacheSwitch>
      </Suspense>
    </>
  );
}
