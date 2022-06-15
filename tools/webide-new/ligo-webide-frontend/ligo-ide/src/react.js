import React from 'react'
import ReactDOM from 'react-dom'

import './scss/index.scss'

import * as serviceWorker from './serviceWorker'
import App from './App'

if (!process.env.CDN) {
  import('./scss/fonts/montserrat/montserrat.css')
  import('./scss/fonts/hack/hack.css')
  import('@fortawesome/fontawesome-free/js/all')
}

document.title = process.env.PROJECT_NAME
ReactDOM.render(<App />, document.getElementById('root'))

window.addEventListener('auxclick', (event) => {
  if (event.button === 1) event.preventDefault()
})

window.__APP_INFO__ = {
  BUILD_TIME: process.env.BUILD_TIME,
  BUILD_ID: process.env.BUILD_ID,
  COMMIT_ID: process.env.COMMIT_ID
}

window.addEventListener('contextmenu', e => e.preventDefault())
// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: http://bit.ly/CRA-PWA
// TODO do we need it or not?
serviceWorker.unregister()
