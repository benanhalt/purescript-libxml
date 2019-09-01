"use strict";

const libxmljs = require('libxmljs');

exports.dtdName = dtd => dtd.name;
exports._dtdExternalId = dtd => dtd.externalId;
exports._dtdSystemId = dtd => dtd.systemId;
