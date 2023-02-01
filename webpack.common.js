const path = require("path");
const {version} = require('./package.json');

module.exports = {
  context: __dirname,
  entry: "./src/index.ts",
  output: {
    path: path.join(__dirname, "dist"),
    filename: "index.js",
    library: 'ribbons',
    globalObject: 'this',
    libraryTarget: 'umd',
  },
  module: {
    rules: [
      {
        exclude: /node_modules/,
        test: /\.tsx?$/,
        use: "ts-loader"
      }
    ]
  },
  mode: 'production',
  resolve: {
    extensions: [".js", ".ts"]
  },
}
