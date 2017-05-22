'use strict';

module.exports.id = "create_tax_rates";

module.exports.up = function (done) {
  var coll = this.db.collection('tax_rates');

  this.db.collection('taxes').findOne({ description: "I.V.A." }, function(err, doc) {
    var id = doc._id.toHexString();
    coll.insert({ code: 0, description: "0%", group: 0, percentage: 0, tax: id });
    coll.insert({ code: 2, description: "12%", group: 0, percentage: 12, tax: id });
    coll.insert({ code: 3, description: "14%", group: 0, percentage: 14, tax: id });
    coll.insert({ code: 6, description: "No objeto de impuesto", group: 0, percentage: 0, tax: id });
    coll.insert({ code: 7, description: "Exento de I.V.A.", group: 0, percentage: 0, tax: id });
  });

  this.db.collection('taxes').findOne({ description: "I.C.E." }, function(err, doc) {
    var id = doc._id.toHexString();

    // Group 1
    coll.insert({ code: 3023, description: "Productos del tabaco y sucedáneos del tabaco",
      percentage: 150, group: 1, tax: id });
    coll.insert({ code: 3610, description: "Perfumes y aguas de tocador",
      percentage: 20, group: 1, tax: id });
    coll.insert({ code: 3620, description: "Videojuegos",
      percentage: 35, group: 1, tax: id });
    coll.insert({ code: 3630, description: "Armas de fuego, armas deportivas y municiones excepto aquellas adquiridas por la fuerza pública",
      percentage: 300, group: 1, tax: id });
    coll.insert({ code: 3640, description: "Focos incandescentes excepto aquellos utilizados como insumos automotrices",
      percentage: 100, group: 1, tax: id });
    coll.insert({ code: 3670, description: "Cocinas, calefones y otros de uso doméstico a gas SRI",
      percentage: 100, group: 1, tax: id });
    coll.insert({ code: 3770, description: "Cocinas, calefones y otros de uso doméstico a gas SENAE",
      percentage: 100, group: 1, tax: id });

    // Group 2
    coll.insert({ code: 3073, description: "Vehículos motorizados cuyo precio de venta al público sea de hasta USD 20.000",
      percentage: 5, group: 2, tax: id });
    coll.insert({ code: 3072, description: "Camionetas, furgonetas, camiones, y vehìculos de rescate cuyo precio de venta al público sea de hasta USD 30.000",
      percentage: 5, group: 2, tax: id });
    coll.insert({ code: 3074, description: "Vehículos motorizados, excepto camionetas, furgonetas, camiones y vehículos de rescate, cuyo precio de venta al público sea superior a USD 20.000 y de hasta USD 30.000",
      percentage: 10, group: 2, tax: id });
    coll.insert({ code: 3075, description: "Vehículos motorizados, cuyo precio de vental al público sea superior a USD 30.000 y de hasta USD 40.000",
      percentage: 15, group: 2, tax: id });
    coll.insert({ code: 3077, description: "Vehículos motorizados, cuyo precio de vental al público sea superior a USD 40.000 y de hasta USD 50.000",
      percentage: 20, group: 2, tax: id });
    coll.insert({ code: 3078, description: "Vehículos motorizados, cuyo precio de vental al público sea superior a USD 50.000 y de hasta USD 60.000",
      percentage: 25, group: 2, tax: id });
    coll.insert({ code: 3079, description: "Vehículos motorizados, cuyo precio de vental al público sea superior a USD 60.000 y de hasta USD 70.000",
      percentage: 30, group: 2, tax: id });
    coll.insert({ code: 3080, description: "Vehículos motorizados, cuyo precio de vental al público sea superior a USD 70.000",
      percentage: 35, group: 2, tax: id });

    coll.insert({ code: 3171, description: "Vehículos híbridos o eléctricos cuyo precio de venta al público sea de hasta USD 35.000",
      percentage: 2, group: 2, tax: id });
    coll.insert({ code: 3172, description: "Vehículos híbridos o eléctricos cuyo precio de venta al público sea superior a USD 35.000 y de hasta USD 40.000",
      percentage: 8, group: 2, tax: id });
    coll.insert({ code: 3173, description: "Vehículos híbridos o eléctricos cuyo precio de venta al público sea superior a USD 40.000 y de hasta USD 50.000",
      percentage: 14, group: 2, tax: id });
    coll.insert({ code: 3174, description: "Vehículos híbridos o eléctricos cuyo precio de venta al público sea superior a USD 50.000 y de hasta USD 60.000",
      percentage: 20, group: 2, tax: id });
    coll.insert({ code: 3175, description: "Vehículos híbridos o eléctricos cuyo precio de venta al público sea superior a USD 60.000 y de hasta USD 70.000",
      percentage: 26, group: 2, tax: id });
    coll.insert({ code: 3176, description: "Vehículos híbridos o eléctricos cuyo precio de venta al público sea superior a USD 70.000",
      percentage: 28, group: 2, tax: id });
    coll.insert({ code: 3081, description: "Aviones, avionetas y helicópteros excepto aquellas destinadas al transporte comercial de pasajeros, carga y servicios; motos acuáticas, tricares, cuadrones, yates y barcos de recreo",
      percentage: 15, group: 2, tax: id });

    // Group 3
    coll.insert({ code: 3092, description: "Servicios de televisión pagada",
      percentage: 15, group: 3, tax: id });
    coll.insert({ code: 3650, description: "Servicios de casinos, salas de juego (bingo - mecánicos) y otros juegos de azar",
      percentage: 35, group: 3, tax: id });
    coll.insert({ code: 3093, description: "Servicios de telefonía",
      percentage: 15, group: 3, tax: id });

    // Group 4
    coll.insert({ code: 3660, description: "Las cuotas, membresías, afiliaciones, acciones y similares que cobren a sus miembros y usuaros los Clubes Sociales, para prestar sus servicios, cuyo moto en su conjunto supere los USD 1.500 anuales",
      percentage: 35, group: 4, tax: id });

    // Group 5
    coll.insert({ code: 3011, description: "Cigarrillos rubio",
      rate: 0.16, unit: 1, group: 5, tax: id });
    coll.insert({ code: 3542, description: "Cigarrillos rubio SENAE",
      rate: 0.16, unit: 1, group: 5, tax: id });
    coll.insert({ code: 3021, description: "Cigarrillos negros",
      rate: 0.16, unit: 1, group: 5, tax: id });
    coll.insert({ code: 3543, description: "Cigarrillos negros SENAE",
      rate: 0.16, unit: 1, group: 5, tax: id });
    coll.insert({ code: 3031, description: "Bebidas alcohólicas, distintas a la cerveza",
      percentage: 75, rate: 7.24, unit: 1, unit_name: "litros de alcohol puro",
      group: 5, tax: id });
    coll.insert({ code: 3041, description: "Cerveza industrial",
      rate: 12, unit: 1, unit_name: "litros de alcohol puro", group: 5, tax: id });
    coll.insert({ code: 3043, description: "Cerveza artesanal",
      percentage: 75, rate: 7.24, unit: 1, unit_name: "litros de alcohol puro",
      group: 5, tax: id });
    coll.insert({ code: 3545, description: "Cerveza artesanal SENAE",
      percentage: 75, rate: 7.24, unit: 1, unit_name: "litros de alcohol puro",
      group: 5, tax: id });
    coll.insert({ code: 3054, description: "Bebidas gaseosas con bajo contenido de azúcar",
      percentage: 10, group: 5, tax: id });
    coll.insert({ code: 3553, description: "Bebidas gaseosas con bajo contenido de azúcar SENAE",
      percentage: 10, group: 5, tax: id });
    coll.insert({ code: 3053, description: "Bebidas gaseosas con alto contenido de azúcar",
      rate: 0.18, unit: 100, unit_name: "gramos de azúcar", group: 5, tax: id });
    coll.insert({ code: 3552, description: "Bebidas gaseosas con alto contenido de azúcar SENAE",
      rate: 0.18, unit: 100, unit_name: "gramos de azúcar", group: 5, tax: id });
    coll.insert({ code: 3101, description: "Bebidas energizantes",
      percentage: 10, group: 5, tax: id });
    coll.insert({ code: 3601, description: "Bebidas energizantes SENAE",
      percentage: 10, group: 5, tax: id });
    coll.insert({ code: 3111, description: "Bebidas no alcohólicas",
      rate: 0.18, unit: 100, unit_name: "gramos de azúcar", group: 5, tax: id });
    coll.insert({ code: 3602, description: "Bebidas no alcohólicas SENAE",
      rate: 0.18, unit: 100, unit_name: "gramos de azúcar", group: 5, tax: id });
    coll.insert({ code: 3531, description: "Bebidas alcohólicas SENAE",
      percentage: 75, rate: 7.24, unit: 1, unit_name: "litro de alcohol puro", group: 5, tax: id });
    coll.insert({ code: 3541, description: "Cerveza industrial SENAE",
      percentage: 75, rate: 12, unit: 1, unit_name: "litro de alcohol puro", group: 5, tax: id });
  });

  done();
};

module.exports.down = function (done) {
  var coll = this.db.collection('tax_rates');
  coll.deleteMany({});
  done();
};
