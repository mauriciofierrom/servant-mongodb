'use strict';

module.exports.id = "create_retention_tax";

module.exports.up = function (done) {
  // use this.db for MongoDB communication, and this.log() for logging
  var coll = this.db.collection("retention_tax");

  coll.insert({ code: 1, description: "Renta" });
  coll.insert({ code: 2, description: "I.V.A." });
  coll.insert({ code: 2, description: "I.S.D." });

  done();
};

module.exports.down = function (done) {
  // use this.db for MongoDB communication, and this.log() for logging
  var coll = this.db.collection("retention_tax");
  coll.deleteMany({});
  done();
};
