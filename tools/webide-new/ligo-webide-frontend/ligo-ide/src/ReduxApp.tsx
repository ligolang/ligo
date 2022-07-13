import { GlobalModals, autoUpdater } from '@obsidians/global';
import React, { lazy, useEffect, useRef, useState } from 'react';
import { config, updateStore } from './lib/redux';
import redux, { Provider } from '@obsidians/redux';

import Auth from '@obsidians/auth';
import { LoadingScreen } from '@obsidians/ui-components';
import { NotificationSystem } from '@obsidians/notification';
import Routes from './components/Routes';
import fileOps, { indexedDBFileSystem, fileSystems, fileSystem } from '@obsidians/file-ops';
import icon from './components/icon.png';

const Header = lazy(() =>
  import('./components/Header' /* webpackChunkName: "header" */)
);

const ReduxApp = (props) => {
  const [loaded, setLoaded] = useState(false)
  const ligoIdeFileSystems = useRef<fileSystems>(new fileSystems())
  const indexedDB = useRef<fileSystem>(new indexedDBFileSystem())

  useEffect(() => {
    async function loadStorage() {
      await redux.init(config, updateStore).then(onReduxLoaded);
      await ligoIdeFileSystems.current.addFileSystem(indexedDB.current)
      await ligoIdeFileSystems.current.setFileSystem([indexedDB.current])
      // TODO in case of any changes in fs we should be able to migrate data
      setLoaded(true)
      autoUpdater.check();
    }
    loadStorage()
  }, [])

  if (!loaded) {
    return <LoadingScreen />;
  }

  return (
    <Provider store={redux.store}>
      <div
        className="body"
        style={{ paddingTop: '49px' }}
      >
        <Routes>
          <Header history={props.history} />
          <NotificationSystem />
          <GlobalModals icon={icon} />
        </Routes>
      </div>
    </Provider>
  );
}

export default ReduxApp

async function onReduxLoaded() {
  Auth.restore();
  const version = await fileOps.getAppVersion();
  redux.dispatch('SET_VERSION', { version });
}
