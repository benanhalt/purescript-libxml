"use strict";

const libxmljs = require('libxmljs');

exports.newComment = content => doc => () => new libxmljs.Comment(doc, content);
exports.commentText = comment => () => comment.text();
exports.commentSetText = content => comment => () => comment.text(content);

