'use strict';

module.exports.id = "create_taxes";

module.exports.up = function (done) {
  var coll = this.db.collection('taxes');
  coll.insert({ code: 2, description: "I.V.A." });
  coll.insert({ code: 3, description: "I.C.E." });
  coll.insert({ code: 5, description: "I.R.B.P.N.R." });
  done();
};

module.exports.down = function (done) {
  // use this.db for MongoDB communication, and this.log() for logging
  var coll = this.db.collection('taxes');
  coll.deleteMany({});
  done();
};
