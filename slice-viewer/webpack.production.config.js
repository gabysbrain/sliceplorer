var path = require('path');
var webpack = require('webpack');
var HtmlWebpackPlugin = require('html-webpack-plugin');

module.exports = {
  entry: [ path.join(__dirname, 'support/index.js') ],
  output: {
    path: path.resolve('./dist'),
    filename: '/js/[name]-[hash].min.js',
    publicPath: '/js/'
  },
  module: {
    loaders: [
      {
        test: /\.purs$/,
        loader: 'purs-loader',
        exclude: /node_modules/,
        query: {
          psc: 'psa',
          bundle: true,
          warnings: false
        }
      },
      { 
        test: /\.scss$/, 
        use: [
          { loader: 'style-loader' }, 
          { loader: 'css-loader' }, 
          {
            loader: 'sass-loader',
            options: {
              includePaths: [
                path.resolve(__dirname, './bower_components/foundation-sites/scss/'),
                path.resolve(__dirname, './bower_components/')
              ]
            }
          }
        ]
      }
    ],
  },
  plugins: [
    new webpack.DefinePlugin({
      'process.env.NODE_ENV': JSON.stringify('production')
    }),
    new webpack.optimize.OccurrenceOrderPlugin(true),
    new webpack.LoaderOptionsPlugin({
      minimize: true,
      debug: false
    }),
    new HtmlWebpackPlugin({
      template: 'html/index.html',
      inject: 'body',
      filename: 'index.html'
    })
  ],
  resolveLoader: {
    modules: [
      path.join(__dirname, 'node_modules')
    ]
  },
  resolve: {
    modules: [
      'node_modules',
      'bower_components'
    ],
    extensions: ['.js', '.purs']
  }
};
