'use strict';

module.exports.id = "create_environmets";

module.exports.up = function (done) {
  // use this.db for MongoDB communication, and this.log() for logging
  var coll = this.db.collection('environments');

  coll.insert({ code: 1, description: "Prueba"});
  coll.insert({ code: 2, description: "Producción"});

  done();
};

module.exports.down = function (done) {
  // use this.db for MongoDB communication, and this.log() for logging
  var coll = this.db.collection('environments');
  coll.deleteMany({});
  done();
};
