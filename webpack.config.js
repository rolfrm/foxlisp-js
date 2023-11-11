const path = require('path');

module.exports = {
	 mode: 'development',
	 entry: './foxlisp-web.js', // Entry point of your application
  output: {
    filename: 'bundle.js', // Output bundle file
    path: path.resolve(__dirname, 'dist'), // Output directory
  },
	 devServer: {
	 static: {
        directory: __dirname,
    },
    compress: true,
    port: 9000,
}
};

