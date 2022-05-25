import { GlobalModals, autoUpdater } from '@obsidians/global';
import React, { Component, lazy } from 'react';
import { config, updateStore } from './lib/redux';
import redux, { Provider } from '@obsidians/redux';

import Auth from '@obsidians/auth';
import { LoadingScreen } from '@obsidians/ui-components';
import { NotificationSystem } from '@obsidians/notification';
import Routes from './components/Routes';
import fileOps from '@obsidians/file-ops';
import icon from './components/icon.png';

const Header = lazy(() =>
  import('./components/Header' /* webpackChunkName: "header" */)
);

export default class ReduxApp extends Component {
  state = {
    loaded: false
  };

  async componentDidMount() {
    await redux.init(config, updateStore).then(onReduxLoaded);
    this.setState({ loaded: true });
    autoUpdater.check();
  }

  render() {
    if (!this.state.loaded) {
      return <LoadingScreen />;
    }

    return (
      <Provider store={redux.store}>
        <div
          className="body"
          style={{ paddingTop: '49px' }}
        >
          <Routes>
            <Header history={this.props.history} />
            <NotificationSystem />
            <GlobalModals icon={icon} />
          </Routes>
        </div>
      </Provider>
    );
  }
}

async function onReduxLoaded() {
  Auth.restore();
  const version = await fileOps.current.getAppVersion();
  redux.dispatch('SET_VERSION', { version });
}
