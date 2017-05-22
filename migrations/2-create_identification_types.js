'use strict';

module.exports.id = "create_identification_types";

module.exports.up = function (done) {
  // use this.db for MongoDB communication, and this.log() for logging
  var coll = this.db.collection('identification_types');
  coll.insert({ code: 4, description: "R.U.C." });
  coll.insert({ code: 5, description: "Cédula" });
  coll.insert({ code: 6, description: "Pasaporte" });
  coll.insert({ code: 7, description: "Venta a consumidor final" });
  coll.insert({ code: 8, description: "Identificación del exterior" });
  coll.insert({ code: 9, description: "Placa" });

  done();
};

module.exports.down = function (done) {
  // use this.db for MongoDB communication, and this.log() for logging
  var coll = this.db.collection('identification_types');
  coll.deleteMany({});
  done();
};
