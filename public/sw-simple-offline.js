// This is the "Simple offline" service worker

var CACHE = "simple-offline-1";

this.addEventListener("activate", function(event) {
  var cacheWhitelist = [ CACHE ];
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

self.addEventListener("install", function (event) {
  event.waitUntil(
    caches.open( CACHE ).then(function (cache) {
      return cache.addAll([
        "offline.html",
        "css/bootstrap.min.css",
        "css/veda.css",
        "favicon.ico",
        "brand.png",
      ]);
    })
  );
});

self.addEventListener("fetch", function (event) {
  if (event.request.method !== "GET") return;
  event.respondWith(
    fetch(event.request)
      .then(function (response) {
        return response;
      })
      .catch(function (error) {
        return caches.open( CACHE ).then(function (cache) {
          if (event.request.destination === "document") {
            return cache.match("offline.html");
          } else {
            return caches.match(event.request).then(function (match) {
              if (match) {
                return match;
              } else {
                throw error;
              }
            });
          }
        });
      })
  );
});
