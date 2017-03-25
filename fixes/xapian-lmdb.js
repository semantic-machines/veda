// xapian != lmdb ?

var xapian_positions_uris = query({
  ticket: veda.ticket,
  query: "'rdf:type'==='v-s:Position' && 'v-s:deleted' == 'true'",
  top: 100000,
  limit: 100000
}).result;

var lmdb_positions = get_individuals(veda.ticket, xapian_positions_uris);

var lmdb_positions_uris = lmdb_positions.map(function (position) {
  return position["@"];
});

var delta = [];

xapian_positions_uris.map(function (position_uri) {
  if (lmdb_positions_uris.indexOf(position_uri) < 0) {
    delta.push( position_uri );
  }
});

console.log( JSON.stringify(delta) );
