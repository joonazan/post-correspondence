const HtmlWebPackPlugin = require("html-webpack-plugin");
module.exports = {
  module: {
    rules: [{
      test: /\.elm$/,
      exclude: [/elm-stuff/, /node_modules/],
      use: {
        loader: 'elm-webpack-loader',
        options: {}
      }
    }]
  },
  plugins: [
    new HtmlWebPackPlugin({template: './index.html'})
  ]
};