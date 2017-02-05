
// module Data.Config

var devMode = process.env.NODE_ENV === 'development';

var devServer = 'http://localhost:5000';
var prodServer = 'http://sliceplore.cs.univie.ac.at';

exports.server = devMode ? devServer : prodServer;


