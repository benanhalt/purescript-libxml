"use strict";

const libxmljs = require('libxmljs');

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
