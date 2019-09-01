"use strict";

const libxmljs = require('libxmljs');

exports.attrName = attr => attr.name();
exports.attrValue = attr => () => attr.value();
exports.attrSetValue = value => attr => () => attr.value(value);
exports.attrRemove = attr => () => attr.remove();
