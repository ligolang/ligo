/* eslint-disable @typescript-eslint/no-unsafe-call */
/* eslint-disable @typescript-eslint/no-unsafe-member-access */
/* eslint-disable react/destructuring-assignment */
/* eslint-disable @typescript-eslint/no-unsafe-assignment */
/* eslint-disable new-cap */
import { lazy, useEffect, useRef, useState } from "react";
import { GlobalHotKeys } from "react-hotkeys";
import { config } from "~/lib/redux";
import redux, { Provider } from "~/base-components/redux";

import { LoadingScreen } from "~/base-components/ui-components";
import { NotificationSystem } from "~/base-components/notification";
import Routes from "./components/Routes";
import fileOps, { indexedDBFileSystem, fileSystems, fileSystem } from "~/base-components/file-ops";
import { ProjectManager, actions } from "~/base-components/workspace";
import LigoHeader from "~/components/LigoHeader";

const Header = lazy(() => import("./components/Header" /* webpackChunkName: "header" */));
const BottomBar = lazy(() => import("./components/BottomBar" /* webpackChunkName: "bottombar" */));

const ReduxApp = (props: { history: any }) => {
  const [loaded, setLoaded] = useState(false);
  const ligoIdeFileSystems = useRef<fileSystems>(new fileSystems());
  const indexedDB = useRef<fileSystem>(new indexedDBFileSystem());
  const bottomBarRef = useRef<typeof BottomBar>(null);

  useEffect(() => {
    async function loadStorage() {
      await redux.init(config);
      await ligoIdeFileSystems.current.addFileSystem(indexedDB.current);
      ligoIdeFileSystems.current.setFileSystem([indexedDB.current]);
      if (!(await fileOps.exists(".workspaces/default-project"))) {
        const Manager = ProjectManager;
        const defaultProject = await Manager.createProject("default-project", "increment", "ligo");
        redux.dispatch("ADD_PROJECT", {
          type: "local",
          project: defaultProject,
        });
      }
      setLoaded(true);
    }
    // eslint-disable-next-line @typescript-eslint/no-floating-promises
    loadStorage();
  }, []);

  if (!loaded) {
    return <LoadingScreen />;
  }

  return (
    <Provider store={redux.store}>
      <div className="body">
        <LigoHeader />
        <Header history={props.history} />
        <NotificationSystem />
        <GlobalHotKeys
          keyMap={{ CtrlCmdB: ["command+b", "control+b"] }}
          handlers={{
            CtrlCmdB: () => {
              if (actions.projectManager !== null) {
                actions.projectManager.compile(null, undefined);
              }
            },
          }}
        />
        <Routes />
        <BottomBar ref={bottomBarRef} />
      </div>
    </Provider>
  );
};

export default ReduxApp;
