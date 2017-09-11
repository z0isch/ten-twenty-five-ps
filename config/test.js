var webpack = require("webpack");
var HtmlWebpackPlugin = require("html-webpack-plugin");
var path = require("path");

module.exports = {
  entry: ["babel-polyfill", path.join(__dirname, "../src/Main.js")],
  output: {
    path: path.join(__dirname, "../build"),
    filename: "bundle.js",
    sourceMapFilename: "bundle.map"
  },
  module: {
    loaders: [
      { test: /\.json$/, loader: "json-loader" },
      {
        test: /\.jsx?$/,
        loader: "babel-loader"
      },
      {
        test: /\.css$/,
        use: [
          { loader: "style-loader" },
          { loader: "css-loader", options: { url: false, import: false } }
        ]
      },
      {
        test: /\.purs$/,
        loader: "purs-loader"
      }
    ]
  },

  plugins: [
    new HtmlWebpackPlugin({
      template: path.join(__dirname, "../index.tmpl.html")
    }),
    new webpack.DefinePlugin({
      "process.env": {
        NODE_ENV: JSON.stringify("test"),
        APP_ID: JSON.stringify("app-c676c3bd-98f5-4662-9600-ae939e247cc7")
      }
    }),
    new webpack.optimize.UglifyJsPlugin({
      beautify: false,
      mangle: {
        screw_ie8: true,
        keep_fnames: true
      },
      compress: {
        screw_ie8: true
      },
      comments: false
    })
  ],
  resolve: {
    modules: ["node_modules", "bower_components"],
    extensions: [".purs", ".js", ".json", ".jsx"]
  }
};
