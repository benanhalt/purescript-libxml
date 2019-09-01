"use strict";

const libxmljs = require('libxmljs');

exports._newDoc = (ver, enc) => new libxmljs.Document(ver, enc);
exports.docChildNodes = doc => () => doc.childNodes();
exports.docEncoding = doc => () => doc.encoding();
exports.docVersion = doc => () => doc.version();
exports.docSetEncoding = enc => doc => () => doc.encoding(enc);
exports.docValidate = xsdDoc => doc => () => doc.validate(xsdDoc);
exports.docFind = xpath => doc => () => doc.find(xpath);
exports.docToString = doc => () => doc.toString();
exports._docNode = (name, content, doc) => doc.node(name, content);
exports._docRoot = doc => doc.root();
exports._docSetRoot = (root, doc) => doc.root(root);
exports._docGetDtd = doc => doc.getDtd();

