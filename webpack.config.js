const { resolve } = require("path");
const webpack = require("webpack");

const { ENV } = process.env;

const publicFolder = resolve("./public");

const isProd = ENV === "production";

const webpackLoader = {
  loader: "elm-webpack-loader",
  options: {
    debug: false,
    optimize: isProd,
    cwd: __dirname,
  },
};

const webpackPlugins = isProd
  ? [webpackLoader]
  : [{ loader: "elm-hot-webpack-loader" }, webpackLoader];

module.exports = {
  mode: isProd ? "production" : "development",
  entry: "./src/index.js",
  devServer: {
    publicPath: "/",
    contentBase: publicFolder,
    port: 8000,
    hotOnly: true,
  },
  output: {
    publicPath: "/",
    path: publicFolder,
    filename: "bundle.js",
  },
  module: {
    rules: [
      {
        test: /\.elm$/,
        exclude: [/elm-stuff/, /node_modules/],
        use: webpackPlugins,
      },
      {
        test: /\.(woff(2)?|otf)$/,
        use: [
          {
            loader: "base64-inline-loader",
            options: {},
          },
        ],
      },
      {
        test: /\.css$/,
        use: ["style-loader", "css-loader"],
      },
    ],
  },
  plugins: [new webpack.NoEmitOnErrorsPlugin()],
};
