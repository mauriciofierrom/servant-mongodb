'use strict';

module.exports.id = "create_emission_types";

module.exports.up = function (done) {
  // use this.db for MongoDB communication, and this.log() for logging
  var coll = this.db.collection("emission_types");

  coll.insert({ code: 1, description: "Emisión normal" });
  coll.insert({ code: 2, description: "Emisión por indisponibilidad del sistema" });

  done();
};

module.exports.down = function (done) {
  // use this.db for MongoDB communication, and this.log() for logging
  var coll = this.db.collection("emission_types");
  coll.deleteMany({});
  done();
};
