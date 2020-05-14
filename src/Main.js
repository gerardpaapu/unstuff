var he = require("he");

exports.decodeHTML = function (str) {
  return he.decode(str);
};
