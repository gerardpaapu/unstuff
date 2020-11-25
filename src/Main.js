var he = require("he");

exports.decodeHTML = function (str) {
  return he.decode(str);
};

exports.localDateNow = function () {
  var date = new Date(Date.now());
  return {
    year: date.getFullYear(),
    month: date.getMonth() + 1,
    date: date.getDate()
  }
};
