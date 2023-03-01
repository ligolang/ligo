const path = require("path");
const webpack = require("webpack");
const HtmlWebpackPlugin = require("html-webpack-plugin");

const distPath = path.resolve(__dirname, "dist");

module.exports = {
  resolve: {
    fallback: {
      path: require.resolve("path-browserify"),
      stream: require.resolve("stream-browserify"),
      util: require.resolve("util"),
      buffer: require.resolve("buffer"),
      fs: require.resolve("browserify-fs"),
      crypto: require.resolve("crypto-browserify"),
    },
  },
  mode: "development",
  entry: {
    app: "./src/app.ts",
  },
  module: {
    rules: [
      {
        test: /\.tsx?$/,
        use: "ts-loader",
        exclude: /node_modules/,
      },
      {
        test: /\.wasm/,
        type: "asset/resource",
        generator: {
          filename: "assets/[name]-[hash][ext][query]",
        },
      },
    ],
  },
  devtool: "inline-source-map",
  devServer: {
    static: distPath,
    hot: true,
  },
  plugins: [
    // Work around for Buffer is undefined:
    // https://github.com/webpack/changelog-v5/issues/10
    new webpack.ProvidePlugin({
      Buffer: ["buffer", "Buffer"],
    }),
    new webpack.DefinePlugin({
      "process.browser": JSON.stringify(true),
      "process.nextTick": "window.setImmediate", //./node_modules/levelup/lib/write-stream.js
      "process.env.NODE_DEBUG": JSON.stringify(process.env.NODE_DEBUG || ""),
      "process.env.NODE_ENV": JSON.stringify(
        process.env.NODE_ENV || "development"
      ),
    }),
    new HtmlWebpackPlugin({
      title: "Ligo.js",
      template: "index.template.html",
    }),
  ],
  output: {
    filename: "[name].bundle.js",
    path: distPath,
    clean: true,
  },
};
