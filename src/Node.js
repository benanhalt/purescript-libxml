"use strict";

const libxmljs = require('libxmljs');

exports._nodeType = node => node.type();
exports._nodeDoc = node => () => node.doc();
exports._nodeParent = node => () => node.parent();
exports._prevSibling = node => () => node.prevSibling();
exports._nextSibling = node => () => node.nextSibling();
exports.nodeLine = node => () => node.line();
exports.nodeRemove = node => () => node.remove();
exports.nodeClone = node => () => node.clone();
exports.nodeToString = node => () => node.toString();
