import express from 'express';
import fs from 'fs';
import { dirname, join } from 'path';

import { compileContractHandler } from './handlers/compile-contract';
import { compileExpressionHandler } from './handlers/compile-expression';
import { deployHandler } from './handlers/deploy';
import { dryRunHandler } from './handlers/dry-run';
import { evaluateValueHandler } from './handlers/evaluate-value';
import { runFunctionHandler } from './handlers/run-function';
import { shareHandler } from './handlers/share';
import { sharedLinkHandler } from './handlers/shared-link';
import { loadDefaultState } from './load-state';
import { errorLoggerMiddleware, loggerMiddleware } from './logger';

var bodyParser = require('body-parser');
var escape = require('escape-html');

const app = express();
const port = 8080;

const appRootDirectory =
  process.env['STATIC_ASSETS'] ||
  dirname(require.resolve('../../client/package.json'));
const appBundleDirectory = join(appRootDirectory, 'build');

app.use(bodyParser.json());
app.use(loggerMiddleware);

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
app.get(
  `/p/:hash([0-9a-zA-Z\-\_]+)`,
  sharedLinkHandler(appBundleDirectory, template)
);
app.post('/api/compile-contract', compileContractHandler);
app.post('/api/compile-expression', compileExpressionHandler);
app.post('/api/dry-run', dryRunHandler);
app.post('/api/share', shareHandler);
app.post('/api/evaluate-value', evaluateValueHandler);
app.post('/api/run-function', runFunctionHandler);
app.post('/api/deploy', deployHandler);

app.use(errorLoggerMiddleware);

app.listen(port, () => {
  console.log(`Listening on: ${port}`);
});
