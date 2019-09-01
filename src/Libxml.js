"use strict";

const libxmljs = require('libxmljs');

exports.libxmljs_version = libxmljs.version;
exports.libxml_version = libxmljs.libxml_version;
exports.libxml_parser_version = libxmljs.libxml_parser_version;

exports._parseXmlString = libxmljs.parseXmlString;
exports._parseHtmlString = libxmljs.parseHtmlString;
exports._parseHtmlFragment = libxmljs.parseHtmlFragment;

exports.memoryUsage = () => libxmljs.memoryUsage();
exports.nodeCount = () => libxmljs.nodeCount();

