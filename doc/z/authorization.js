// AUTHORIZATION

var C = 1,
  R = 2,
  U = 4,
  D = 8;

var admin_ticket = _get_admin_ticket();

// MAIN FUNCTION

function authorize(subject_uri, object_uri, right) {

  var object_groups = get_resource_groups(object_uri),
    subject_groups = get_resource_groups(subject_uri);

  for (var obj_key in object_groups) {

    var permissions = get_permissions(obj_key);

    for (var perm_key in permissions) {

      if (perm_key in subject_groups) {

        var list = object_groups[obj_key],
          length = list.length;

        for (var i = 0; i < length; i++ ) {
          if ( (list[i] & permissions[perm_key]) & right ) {
            return true;
          }
        }

      }
    }

  }
  return false;
}

// UTILS

function get_permissions(resource_uri) {

  return _get_individual(admin_ticket, resource_uri + "_p");

}

function get_resource_groups(resource_uri) {

  var result = {};
  result[resource_uri] = [C|R|U|D];
  return _get_resource_groups(resource_uri, result, C|R|U|D);

}

function _get_resource_groups(resource_uri, acc, rights) {

  if (resource_uri === "v-s:AllResourcesGroup") return;

  var membership_uri = resource_uri + "_m",
    membership = _get_individual(admin_ticket, membership_uri);

  if (!membership) return;

  for (var key in membership) {
    acc[key] = acc[key] || [];
    rights = membership[key] & rights;
    acc[key].push( rights );
    _get_resource_groups(key, acc, rights);
  }

  return acc;
}

// TEST DATA

function _get_admin_ticket() {
  return 1;
}

function _get_individual(ticket, uri) {

  get_count++;

  get_stats[uri] = get_stats[uri] ? ++get_stats[uri] : 1;

  var result;

  switch (uri) {

    // objects memberships
    case "add1_m" :
      result = {
        "v-s:AllResourcesGroup" : C|R|U|D,
        "im1": C|R|U|D
      }; break;
    case "ver1_m" :
      result = {
        "v-s:AllResourcesGroup" : C|R|U|D,
        "im1": R
      }; break;
    case "im1_m" :
      result = {
        "v-s:AllResourcesGroup" : C|R|U|D,
        "imc": C|R|U|D
      }; break;
    case "imc_m" :
      result = {
        "v-s:AllResourcesGroup" : C|R|U|D,
        "doc": C|R|U|D
      }; break;
    case "doc_m" :
      result = {
        "v-s:AllResourcesGroup" : C|R|U|D
      }; break;

    // subjects memberships
    case "p1_m" :
      result = {
        "v-s:AllResourcesGroup" : C|R|U|D,
        "pg1" : C|R|U|D,
        "pg2" : C|R|U|D
      }; break;
    case "pg1_m" :
      result = {
        "v-s:AllResourcesGroup" : C|R|U|D,
        "mnd" : C|R|U|D
      }; break;
    case "pg2_m" :
      result = {
        "v-s:AllResourcesGroup" : C|R|U|D,
        "mnd" : C|R|U|D
      }; break;
    case "mnd_m" :
      result = {
        "v-s:AllResourcesGroup" : C|R|U|D
      }; break;

    // object permissions
    case "im1_p" :
      result = {
        "p1" : C|R|U
      }; break;
    /*case "imc_p" :
      result = {
        "mnd" : R
      }; break;
    case "doc_p" :
      result = {
        "mnd" : C|R|U
      }; break;
    case "ver1_p" :
      result = {
        "p1" : C|R|U|D
      }; break;
    */
  }

  return result;
}


// TEST

var get_count = 0;
var get_stats = {};

// p1 -> im1
get_count = 0;
console.log("authorize('p1', 'im1', C)", authorize('p1', 'im1', C), "get_count", get_count);
get_count = 0;
console.log("authorize('p1', 'im1', R)", authorize('p1', 'im1', R), "get_count", get_count);
get_count = 0;
console.log("authorize('p1', 'im1', U)", authorize('p1', 'im1', U), "get_count", get_count);
get_count = 0;
console.log("authorize('p1', 'im1', D)", authorize('p1', 'im1', D), "get_count", get_count);

// p1 -> add1
get_count = 0;
console.log("authorize('p1', 'add1', C)", authorize('p1', 'add1', C), "get_count", get_count);
get_count = 0;
console.log("authorize('p1', 'add1', R)", authorize('p1', 'add1', R), "get_count", get_count);
get_count = 0;
console.log("authorize('p1', 'add1', U)", authorize('p1', 'add1', U), "get_count", get_count);
get_count = 0;
console.log("authorize('p1', 'add1', D)", authorize('p1', 'add1', D), "get_count", get_count);

// p1 -> ver1
get_count = 0;
console.log("authorize('p1', 'ver1', C)", authorize('p1', 'ver1', C), "get_count", get_count);
get_count = 0;
console.log("authorize('p1', 'ver1', R)", authorize('p1', 'ver1', R), "get_count", get_count);
get_count = 0;
console.log("authorize('p1', 'ver1', U)", authorize('p1', 'ver1', U), "get_count", get_count);
get_count = 0;
console.log("authorize('p1', 'ver1', D)", authorize('p1', 'ver1', D), "get_count", get_count);

console.log("get_stats", get_stats);
