// This is the "Simple offline" service worker

var FILES = "files-0";
var STATIC = "static-0";

this.addEventListener("activate", function(event) {
  var cacheWhitelist = [ FILES, STATIC ];
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

self.addEventListener("fetch", function (event) {
  var url = new URL(event.request.url);
  var type = url.pathname.indexOf("/ping") === 0 ? "PING" : url.pathname.indexOf("/api") === 0 ? "API" : url.pathname.indexOf("/files") === 0 ? "FILES" : "STATIC";
  if (event.request.method === "GET") {
    if (type === "PING") {
      event.respondWith(fetch(event.request));
    } else if (type === "STATIC") {
      event.respondWith(handleFetch(event, STATIC));
    } else if (type === "FILES") {
      event.respondWith(handleFetch(event, FILES));
    }
  }
});

function handleFetch(event, CACHE) {
  return caches.match(event.request).then(function(resp) {
    return resp || fetch(event.request).then(function(response) {
      return caches.open( CACHE ).then(function(cache) {
        cache.put(event.request, response.clone());
        return response;
      });
    });
  });
}
