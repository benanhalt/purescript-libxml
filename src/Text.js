"use strict";

const libxmljs = require('libxmljs');

exports.newText = content => doc => () => new libxmljs.Text(doc, content);
exports.textGetText = node => () => node.text();
exports.textSetText = text => node => () => node.text(text);
