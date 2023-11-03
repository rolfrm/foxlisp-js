const path = require('path');

module.exports = {
	 mode: 'development',
	 entry: './lispy.js', // Entry point of your application
  output: {
    filename: 'bundle.js', // Output bundle file
    path: path.resolve(__dirname, 'dist'), // Output directory
  },
};
