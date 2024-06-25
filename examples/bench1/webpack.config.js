const path = require('path');
const CopyWebpackPlugin = require('copy-webpack-plugin');
const HtmlWebpackPlugin = require('html-webpack-plugin');
module.exports = {
	 mode: 'development',
	 entry: '../../src/foxlisp-web.js', // Entry point of your application
   watch: true,
  output: {
    filename: 'bundle.js', // Output bundle file
    path: path.resolve("dist"), // Output directory
    publicPath: "/",
  },
  module: {
    rules: [
      {
        test: /\.\.\/\.\.\/lisp\/\.(lisp)$/i,
        type: "asset/resource"
      }
    ]
  },
  plugins: [
    new CopyWebpackPlugin({patterns: [
      { from: '../../lisp', to: 'lisp' },
      { from: '*.lisp', to: 'lisp' },
      //css
      { from: 'styles.css', to: '' },
    ]}),
    new HtmlWebpackPlugin({
      title: 'Foxlisp JS Bench 1',
      template: './index.html', // Path to your HTML template
      filename: 'index.html'
    }),
  ],
	 devServer: {
    static: './dist',
    compress: false,
    hot: true,
    port: 9000,
}
};

