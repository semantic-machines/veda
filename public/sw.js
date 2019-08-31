var STATIC = "static-2";
var API = "api-1";

this.addEventListener("activate", function(event) {
  var cacheWhitelist = [ STATIC, API ];

  event.waitUntil(
    caches.keys().then(function(keyList) {
      return Promise.all(keyList.map(function(key) {
        if (cacheWhitelist.indexOf(key) === -1) {
          return caches.delete(key);
        }
      }));
    })
  );
});

var api_fns = {
  // GET
  'authenticate':'{"end_time":' + (Date.now() + 12 * 3600 * 1000) + ',"id":"","result":200,"user_uri":""}',
  'get_ticket_trusted':'',
  'is_ticket_valid':'true',
  'get_rights':'{"@":"_","rdf:type":[{"data":"v-s:PermissionStatement","type":"Uri"}],"v-s:canCreate":[{"data":true,"type":"Boolean"}],"v-s:canDelete":[{"data":false,"type":"Boolean"}],"v-s:canRead":[{"data":true,"type":"Boolean"}],"v-s:canUpdate":[{"data":true,"type":"Boolean"}]}',
  'get_rights_origin':'',
  'get_membership':'{"@":"_","rdf:type":[{"data":"v-s:Membership","type":"Uri"}],"v-s:memberOf":[{"data":"v-s:AllResourcesGroup","type":"Uri"}]}',
  'get_individual':'{"@": "$$$","rdf:type":[{"type":"Uri","data": "rdfs:Resource"}],"rdfs:label": [{"type": "String", "data": "Вы работаете офлайн. Этот объект сейчас недоступен.", "lang": "RU"},{"type": "String", "data": "You are offline. This object is not available now.", "lang": "EN"}]}',
  'reset_individual':'{"@": "$$$","rdf:type":[{"type":"Uri","data": "rdfs:Resource"}],"rdfs:label": [{"type": "String", "data": "Вы работаете офлайн. Этот объект сейчас недоступен.", "lang": "RU"},{"type": "String", "data": "You are offline. This object is not available now.", "lang": "EN"}]}',

  // POST
  'query':'{"result":[],"count":0,"estimated":0,"processed":0,"cursor":0,"result_code":200}',
  'get_individuals':'[]',

  // PUT
  'remove_individual':'{"op_id":0,"result":200}',
  'put_individual':'{"op_id":0,"result":200}',
  'add_to_individual':'{"op_id":0,"result":200}',
  'set_in_individual':'{"op_id":0,"result":200}',
  'remove_from_individual':'{"op_id":0,"result":200}',
  'put_individuals':'{"op_id":0,"result":200}'
};

var re = /.*\/\/[^\/]*\/([^\/#?]*)/;

this.addEventListener("fetch", function(event) {
  var fn = event.request.url.match(re)[1];
  var isApi = fn in api_fns;
  event.respondWith( isApi ? getApiResponse(event, fn) : getStaticResource(event) );
});

function getStaticResource(event) {
  return caches.match(event.request).then(function(resp) {
    return resp || fetch(event.request).then(function(response) {
      return caches.open( STATIC ).then(function(cache) {
        cache.put(event.request, response.clone());
        return response;
      });
    });
  });
}

var currentStatus = navigator.onLine;

function getApiResponse(event, fn) {
  var cloneRequest = event.request.method === "GET" ? undefined : event.request.clone();
  return new Promise(function (resolve, reject) {
    if (navigator.onLine) {
      resolve(flushQueue());
    } else {
      reject();
    }
    if (currentStatus !== navigator.onLine) {
      currentStatus = navigator.onLine;
      send_message(currentStatus ? "online" : "offline");
    }
  })
  .then(function () {
    return fetch(event.request).then(function(response) {
      if (event.request.method === "GET") {
        return caches.open( API ).then(function(cache) {
          cache.put(event.request, response.clone());
          return response;
        });
      } else if (event.request.method === "POST") {
        var db = new LocalDB();
        return db.then(function (db) {
          Promise.all([serialize(cloneRequest), serialize(response)]).then(function (req_res) {
            var request = JSON.stringify(req_res[0]);
            var response = req_res[1];
            db.put(request, response);
          });
          return response;
        });
      } else {
        return response;
      }
    });
  })
  .catch(function (err) {
    if (event.request.method === "GET") {
      return caches.match(event.request).then(function (match) {
        if (match) {
          return match;
        } else {
          return new Response(api_fns[fn], { headers: { "Content-Type": "application/json" } });
        }
      });
    } else if (event.request.method === "POST") {
      return serialize(cloneRequest).then(function (request) {
        request = JSON.stringify(request);
        var db = new LocalDB();
        return db.then(function (db) {
          return db.get(request).then(deserialize);
        }).catch(function (err) {
          return new Response(api_fns[fn], { headers: { "Content-Type": "application/json" } });
        });
      });
    } else if (event.request.method === "PUT") {
      return enqueueRequest(cloneRequest).then(function (queue) {
        console.log("Offline operation added to queue, queue length = ", queue.length);
        return new Response(api_fns[fn], { headers: { "Content-Type": "application/json" } });
      });
    } else {
      return err;
    }
  });
}

// Offline PUT queue
function enqueueRequest(request) {
  var db = new LocalDB();
  return db.then(function (db) {
    return serialize(request).then(function (serializedRequest) {
      return db.get("offline-queue").then(function(queue) {
        queue = queue || [];
        queue.push(serializedRequest);
        return db.put("offline-queue", queue);
      });
    });
  });
}
function flushQueue() {
  var db = new LocalDB();
  return db.then(function (db) {
    return db.get("offline-queue").then(function(queue) {
      if (queue && queue.length) {
        return queue.reduce(function (prom, request) {
          return prom.then(function () {
            return deserialize(request);
          }).then(function (request) {
            return fetch(request);
          });
        }, Promise.resolve()).then(function () {
          console.log("Back online! Queued requests sent to server.");
          return db.remove("offline-queue");
        });
      } else {
        return Promise.resolve();
      }
    });
  });
}

function send_message_to_client(client, msg) {
  var channel = new MessageChannel();
  client.postMessage(msg, [channel.port2]);
}
function send_message(msg){
  clients.matchAll().then(function (clients) {
    clients.forEach(function (client) {
      send_message_to_client(client, msg);
    });
  });
}

// indexedDB for non-GET requests
var db_name = "veda-sw";
var store = "sw";

var fallback = {
  get: function (key) {
    return Promise.resolve(this[key]);
  },
  put: function (key, value) {
    this[key] = value;
    return Promise.resolve(value);
  },
  remove: function (key) {
    var result = delete this[key];
    return Promise.resolve(result);
  }
};

var LocalDB = function () {

  var self = this;

  if (LocalDB.prototype._singletonInstance) {
    return Promise.resolve(LocalDB.prototype._singletonInstance);
  }

  return LocalDB.prototype._singletonInstance = initDB();

  function initDB() {

    return new Promise(function (resolve, reject) {

      var openReq = indexedDB.open(db_name, 1);

      openReq.onsuccess = function (event) {
        var db = event.target.result;
        self.db = db;
        console.log("DB open success");
        resolve(self);
      };

      openReq.onerror = function errorHandler(error) {
        console.log("DB open error", error);
        reject(error);
      };

      openReq.onupgradeneeded = function (event) {
        var db = event.target.result;
        db.createObjectStore(store);
        console.log("DB create success");
      };
    }).catch(function (error) {
      console.log("IndexedDB error, using in-memory fallback.\n", error);
      return fallback;
    });
  }
};

var proto = LocalDB.prototype;

proto.get = function (key) {
  var self = this;
  return new Promise(function (resolve, reject) {
    var request = self.db.transaction([store], "readonly").objectStore(store).get(key);
    request.onerror = function(error) {
      reject(error);
    };
    request.onsuccess = function(event) {
      resolve(request.result);
    };
  });
};

proto.put = function (key, value) {
  var self = this;
  return new Promise(function (resolve, reject) {
    var request = self.db.transaction([store], "readwrite").objectStore(store).put(value, key);
    request.onerror = function(error) {
      reject(error);
    };
    request.onsuccess = function(event) {
      resolve(value);
    };
  });
};

proto.remove = function (key) {
  var self = this;
  return new Promise(function (resolve, reject) {
    var request = self.db.transaction([store], "readwrite").objectStore(store).delete(key);
    request.onerror = function(error) {
      reject(error);
    };
    request.onsuccess = function(event) {
      resolve(request.result);
    };
  });
};

function serialize(subject) {
  var headers = {};
  for (var entry of subject.headers.entries()) {
    headers[entry[0]] = entry[1];
  }
  var serialized;
  if (subject instanceof Request) {
    serialized = {
      url: subject.url,
      headers: headers,
      method: subject.method,
      mode: subject.mode,
      credentials: subject.credentials,
      cache: subject.cache,
      redirect: subject.redirect,
      referrer: subject.referrer
    };
  } else {
      serialized = {
      url: subject.url,
      headers: headers,
      ok: subject.ok,
      redirected: subject.redirected,
      status: subject.status,
      statusText: subject.statusText,
      type: subject.type
    };
  }
  return subject.clone().text().then(function(body) {
    serialized.body = body;
    return serialized;
  });
}

function deserialize(subject) {
  return subject.method ? new Request(subject.url, subject) : new Response(subject.body, subject);
}
