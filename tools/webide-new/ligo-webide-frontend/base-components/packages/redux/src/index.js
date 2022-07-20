import { createStore, combineReducers, applyMiddleware } from 'redux'
import Immutable from 'immutable';
import { persistStore, persistReducer } from 'redux-persist'
import storage from 'redux-persist/lib/storage'
import immutableTransform from 'redux-persist-transform-immutable'
import configureRedux from 'redux-config'
import { composeWithDevTools } from "redux-devtools-extension/developmentOnly";
import middlewares from './middlewares'

export { Provider } from 'react-redux'
export { connect } from './connect'

const composeEnhancers = composeWithDevTools({
  serialize: {
    immutable: Immutable,
  }
});

class Redux {
  constructor () {
    this.store = null
    this.actions = null
  }

  init (config, updateStore) {
    const { actions, reducers, persists } = configureRedux(config)
    // console.info('[Redux] store to persist', persists)

    this.actions = actions
    this.store = createStore(
      persistReducer(
        {
          key: 'root',
          storage,
          whitelist: persists,
          transforms: [immutableTransform()]
        },
        combineReducers(reducers)
      ),
      composeEnhancers(applyMiddleware(...middlewares))
    )

    return new Promise(resolve => {
      const persistor = persistStore(this.store, null, () => {
        // update store due to app update
        updateStore({
          persistor,
          store: this.store,
          actions: this.actions
        })
        resolve(this.getState())
      })
      // persistor.purge();
    })
  }

  getState () {
    return this.store.getState()
  }

  dispatch (action, params) {
    this.store.dispatch(this.actions[action](params))
  }
}

export default new Redux()
