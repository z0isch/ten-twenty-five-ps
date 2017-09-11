var webpack = require("webpack");
var HtmlWebpackPlugin = require("html-webpack-plugin");
var path = require("path");

module.exports = {
  devtool: "eval-source-map",
  entry: ["babel-polyfill", path.join(__dirname, "../src/Main.js")],
  output: {
    path: path.join(__dirname, "../build"),
    filename: "bundle.js"
  },
  module: {
    loaders: [
      {
        test: /\.purs$/,
        loader: "purs-loader"
      },
      { test: /\.jsx?$/, loader: "babel-loader" },
      { test: /\.json$/, loader: "json-loader" },
      {
        test: /\.css$/,
        use: [
          { loader: "style-loader" },
          { loader: "css-loader", options: { url: false, import: false } }
        ]
      }
    ]
  },
  plugins: [
    new HtmlWebpackPlugin({
      template: path.join(__dirname, "../index.tmpl.html")
    }),
    new webpack.HotModuleReplacementPlugin(),
    new webpack.DefinePlugin({
      "process.env": {
        NODE_ENV: JSON.stringify("dev"),
        APP_ID: JSON.stringify("app-c676c3bd-98f5-4662-9600-ae939e247cc7")
      }
    })
  ],
  resolve: {
    modules: ["node_modules", "bower_components"],
    extensions: [".purs", ".js", ".json", ".jsx"]
  },
  devServer: {
    historyApiFallback: true,
    inline: true,
    hot: true
  }
};
