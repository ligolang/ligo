const path = require('path');
const TerserPlugin = require('terser-webpack-plugin');
const {
  override,
  addWebpackExternals,
  addWebpackAlias,
  addWebpackPlugin,
  addExternalBabelPlugins
} = require('customize-cra');
const MonacoWebpackPlugin = require('monaco-editor-webpack-plugin');
const BundleAnalyzerPlugin =
  require('webpack-bundle-analyzer').BundleAnalyzerPlugin;
const { BundleStatsWebpackPlugin } = require('bundle-stats-webpack-plugin');

function findWebpackPlugin(plugins, pluginName) {
  return plugins.find((plugin) => plugin.constructor.name === pluginName);
}

function overrideProcessEnv(value) {
  return (config) => {
    const plugin = findWebpackPlugin(config.plugins, 'DefinePlugin');
    const processEnv = plugin.definitions['process.env'] || {};
    plugin.definitions['process.env'] = {
      ...processEnv,
      ...value,
    };
    return config;
  };
}

function turnOffMangle() {
  return (config) => {
    config.optimization.minimizer = config.optimization.minimizer.map(
      (minimizer) => {
        if (minimizer instanceof TerserPlugin) {
          minimizer.options.terserOptions.mangle = false;
        }
        return minimizer;
      }
    );
    return config;
  };
}

function addWasmLoader(options) {
  return (config) => {
    config.resolve.extensions.push('.wasm');
    config.module.rules.forEach((rule) => {
      (rule.oneOf || []).forEach((oneOf) => {
        if (oneOf.loader && oneOf.loader.indexOf('file-loader') >= 0) {
          oneOf.exclude.push(/\.wasm$/);
        }
      });
    });
    config.module.rules.push({
      test: /node_modules[\\/]onigasm[\\/]lib[\\/]onigasm\.wasm$/,
      loader: "file-loader",
      type: "javascript/auto",
    })
    return config;
  };
}

function customSplitting() {
  return (config) => {
    config.optimization = {
      splitChunks: {
        chunks: 'all',
        cacheGroups: {
          vendor: {
            name(module) {
              const packageName = module.context.match(
                /[\\/]node_modules[\\/](.*?)([\\/]|$)/
              )[1];
              return `vendor.${packageName.replace('@', '')}`;
            },
            test: /[\\/]node_modules[\\/]/,
            chunks: 'all',
          },
          common: {
            test: /[\\/]src[\\/]components[\\/]/,
            chunks: 'async',
          },
        },
      },
    };
    return config;
  };
}

function enableTS() {
  return (config) => {
    const tsRule = config.module.rules[1].oneOf[2];
    tsRule.include = undefined;
    tsRule.exclude = /node_modules/;

    return config;
  };
}

const overrides = [
  addWebpackAlias({
    "~": path.resolve(__dirname, "src/"),
    'vscode/services': require.resolve('vscode/services')
  }),
  overrideProcessEnv({
    PROJECT_NAME: '"LIGO IDE"',
    MEASUREMENT_ID: JSON.stringify(process.env.MEASUREMENT_ID),
    GIT_PROXY: JSON.stringify(process.env.GIT_PROXY),
    HTTP_PROTOCOL: JSON.stringify(process.env.HTTP_PROTOCOL),
    BACKEND_URL: JSON.stringify(process.env.BACKEND_URL),
  }),
  addExternalBabelPlugins("@babel/plugin-proposal-class-properties", "@babel/plugin-proposal-optional-chaining", "@babel/plugin-proposal-nullish-coalescing-operator"),
  enableTS(),
  turnOffMangle(),
  addWasmLoader(),
  customSplitting(),
];


overrides.push(
  addWebpackPlugin(
    new MonacoWebpackPlugin({
      languages: [
        'json',
        'javascript',
        'typescript',
        'css',
        'html',
        'markdown',
        'c',
        'cpp',
        'shell',
      ],
    }),
    new BundleAnalyzerPlugin(),
    new BundleStatsWebpackPlugin()
  )
);
overrides.push(addWebpackExternals({
  'Config': JSON.stringify(require('./config.json')),
}))

module.exports = {
  webpack: override(...overrides),
  devServer: function (configFunction) {
    return function (proxy, allowedHost) {
      const config = configFunction({
        '/api': {
          target: 'http://127.0.0.1:8080',
          pathRewrite: { '^/api': '' },
        },
      }, allowedHost);
      config.headers = {
        'Cross-Origin-Opener-Policy': 'same-origin',
        'Cross-Origin-Embedder-Policy': 'require-corp',
      };
      return config;
    };
  },
};
