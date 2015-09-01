'use strict';

var webpack = require('webpack');

module.exports = {
    entry: {
        crud: './index.js'
    },
    output: {
        path: __dirname + '/crud/jvm/src/main/resources',
        filename: '[name]-deps.js'
    },
    plugins: [
        new webpack.NoErrorsPlugin(),
        new webpack.optimize.CommonsChunkPlugin({
            name: "crud"
        })
    ]
};