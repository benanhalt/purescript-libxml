"use strict";

const libxmljs = require('libxmljs');

exports.attrName = attr => attr.name();
exports.attrValue = attr => () => attr.value();
exports.attrSetValue = value => attr => () => attr.value(value);
exports.attrNode = attr => () => attr.node();
exports.attrSetNameSpace = prefix => href => attr => () => attr.namespace(prefix, href);
exports._attrNameSpace = attr => () => attr.namespace();

