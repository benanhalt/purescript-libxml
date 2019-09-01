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

exports._nodeType = node => node.type();
exports._nodeDoc = node => () => node.doc();
exports._nodeParent = node => () => node.parent();
exports._prevSibling = node => () => node.prevSibling();
exports._nextSibling = node => () => node.nextSibling();
exports.nodeLine = node => () => node.line();
exports.nodeRemove = node => () => node.remove();
exports.nodeClone = node => () => node.clone();
exports.nodeToString = node => () => node.toString();

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

exports.attrName = attr => attr.name();
exports.attrValue = attr => () => attr.value();
exports.attrSetValue = value => attr => () => attr.value(value);
exports.attrRemove = attr => () => attr.remove();

exports.newComment = content => doc => () => new libxmljs.Comment(doc, content);
exports.commentText = comment => () => comment.text();
exports.commentSetText = content => comment => () => comment.text(content);

exports.elementAddNode = name => content => element => () => element.node(name, content);
exports.elementName = element => () => element.name();
exports.elementSetName = name => element => () => element.name(name);
exports.elementText = element => () => element.text();
exports.elementSetText = text => element => () => element.text(text);
exports._elementAttr = name => element => () => element.attr(name);
exports.elementSetAttr = name => value => element => () => element.attr(name, value);
exports.elementAttrs = element => () => element.attrs();
exports.elementChildNodes = element => () => element.childNodes();
exports.elementAddChild = child => element => () => element.addChild(child);
exports._elementNextElement = element => () => element.nextElement();
exports._elementPrevElement = element => () => element.prevElement();
exports.elementAddNextSibling = siblingNode => element => () => element.addNextSibling(siblingNode);
exports.elementAddPrevSibling = siblingNode => element => () => element.addPrevSibling(siblingNode);
exports.elementFind = xpath => element => () => element.find(xpath);
exports.elementReplaceWithElement = replacement => element => () => element.replace(replacement);
exports.elementReplaceWithText = text => element => () => element.replace(text);
exports.elementPath = element => () => element.path();

exports.dtdName = dtd => dtd.name;
exports._dtdExternalId = dtd => dtd.externalId;
exports._dtdSystemId = dtd => dtd.systemId;
