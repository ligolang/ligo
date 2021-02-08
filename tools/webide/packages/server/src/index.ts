import cors from 'cors';
import express from 'express';
import fs from 'fs';
import { dirname, join } from 'path';

import { compileContractHandler } from './handlers/compile-contract';
import { compileExpressionHandler } from './handlers/compile-expression';
import { compileStorageHandler } from './handlers/compile-storage';
import { deployHandler } from './handlers/deploy';
import { dryRunHandler } from './handlers/dry-run';
import { evaluateValueHandler } from './handlers/evaluate-value';
import { runFunctionHandler } from './handlers/run-function';
import { shareHandler } from './handlers/share';
import { sharedLinkHandler } from './handlers/shared-link';
import { loadDefaultState } from './load-state';
import { errorLoggerMiddleware, loggerMiddleware } from './logger';
require('./metrics');

const bodyParser = require('body-parser');
const escape = require('escape-html');
const prometheus = require('express-prometheus-middleware');

const app = express();
const APP_PORT = 8080;

const metrics = express();
const METRICS_PORT = 8081;

const corsOptions = {
  origin: [
    'https://ligolang.org',
    'http://localhost:3000',
    'http://localhost:1234',
  ],
  optionsSuccessStatus: 200,
};

const appRootDirectory =
  process.env['STATIC_ASSETS'] ||
  dirname(require.resolve('../../client/package.json'));
const appBundleDirectory = join(appRootDirectory, 'build');

app.use(bodyParser.json());
app.use(loggerMiddleware);
app.use(
  prometheus({
    metricsPath: '/metrics',
    collectDefaultMetrics: true,
    collectDefaultBuckets: true,
    requestDurationBuckets: [0.5, 0.6, 0.7, 1, 10, 20, 30, 60],
    metricsApp: metrics,
  })
);

const file = fs.readFileSync(join(appBundleDirectory, 'index.html'));

const template = (defaultState: string = '{}') => {
  return file.toString().replace(
    `<div id="root"></div>`,
    // Injecting a script that contains a default state (Might want to refactor this if we do ssr)
    // Adding an div containing the initial state this is vulnerable to xss
    // To avoid vulnerability we escape it and then parse the content into a global variable
    `
     <input type="hidden" id="initialState" value="${escape(defaultState)}" />
     <div id="root"></div>
     <script>var defaultServerState = JSON.parse(document.getElementById("initialState").value); document.getElementById("initialState").remove()</script>`
  );
};
app.use('^/$', async (_, res) =>
  res.send(template(JSON.stringify(await loadDefaultState(appBundleDirectory))))
);
app.use(express.static(appBundleDirectory));

app.options('/api/share', cors(corsOptions));

app.get(`/api/share/:hash([0-9a-zA-Z\-\_]+)`, sharedLinkHandler());
app.post('/api/compile-contract', compileContractHandler);
app.post('/api/compile-expression', compileExpressionHandler);
app.post('/api/compile-storage', compileStorageHandler);
app.post('/api/dry-run', dryRunHandler);
app.post('/api/share', cors(corsOptions), shareHandler);
app.post('/api/evaluate-value', evaluateValueHandler);
app.post('/api/run-function', runFunctionHandler);
app.post('/api/deploy', deployHandler);

app.use(errorLoggerMiddleware);

app.listen(APP_PORT, () => {
  console.log(`API listening on: ${APP_PORT}`);
});

metrics.listen(METRICS_PORT, () => {
  console.log(`Metrics listening on: ${METRICS_PORT}`);
});
