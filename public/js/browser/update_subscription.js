/**

Autoupdate subscription service for individuals that were changed on server

 */

veda.Module(function UpdateService(veda) { "use strict";

  veda.UpdateService = function () {

    // Singleton pattern
    if (veda.UpdateService.prototype._singletonInstance) {
      return veda.UpdateService.prototype._singletonInstance;
    }
    veda.UpdateService.prototype._singletonInstance = this;

    var self = riot.observable(this);

    var address = "ws://" + location.hostname + ":8088/ccus",
        socket,
        msgInterval,
        msgDelay = 1000,
        connectInterval,
        connectDelay = 10000,
        list = {},
        delta = {};

    this.list = function () {
      return list;
    }

    this.synchronize = function() {
      clearInterval(msgInterval);
      msgInterval = undefined;
      list = {};
      delta = {};
      socket.send("=");
      //console.log("client -> server: =");
    }

    this.subscribe = function(uri) {
      if (list[uri]) {
        ++list[uri].subscribeCounter;
        return;
      }
      var individual = new veda.IndividualModel(uri);
      var updateCounter = individual.hasValue("v-s:updateCounter") ? individual["v-s:updateCounter"][0] : 0;
      list[uri] = {
        subscribeCounter: 1,
        updateCounter: updateCounter
      };
      delta[uri] = {
        operation: "+",
        updateCounter: updateCounter
      };
      if (!msgInterval) {
        msgInterval = setInterval(pushDelta, msgDelay);
      }
    }

    this.unsubscribe = function (uri) {
      if (uri === "*") {
        clearInterval(msgInterval);
        msgInterval = undefined;
        list = {};
        delta = {};
        socket.send("-*");
        //console.log("client -> server: -*");
      } else {
        if ( !list[uri] ) {
          return;
        } else if ( list[uri].subscribeCounter === 1 ) {
          delete list[uri];
          delta[uri] = {
            operation: "-"
          };
          if (!msgInterval) {
            msgInterval = setInterval(pushDelta, msgDelay);
          }
        } else {
          --list[uri].subscribeCounter;
          return;
        }
      }
    }

    function pushDelta() {
      var subscribe = [],
          unsubscribe = [],
          subscribeMsg,
          unsubscribeMsg;
      for (var uri in delta) {
        if (delta[uri].operation === "+") {
          subscribe.push("+" + uri + "=" + delta[uri].updateCounter);
        } else {
          unsubscribe.push("-" + uri);
        }
      }
      subscribeMsg = subscribe.join(",");
      unsubscribeMsg = unsubscribe.join(",");
      delta = {};
      if (subscribeMsg) {
        socket.send(subscribeMsg);
        //console.log("client -> server:", subscribeMsg);
      }
      if (unsubscribeMsg) {
        socket.send(unsubscribeMsg);
        //console.log("client -> server:", unsubscribeMsg);
      }
      clearInterval(msgInterval);
      msgInterval = undefined;
    }

    socket = initSocket();

    return this;

    function initSocket () {
      var socket = new WebSocket(address);
      var self = this;
      socket.onopen = openedHandler;
      socket.onclose = closedHandler;
      socket.onerror = closedHandler;
      socket.onmessage = messageHandler;
      return socket;
    }

    function openedHandler(event) {
      clearInterval(connectInterval);
      connectInterval = undefined;
      //console.log("client: socket opened", event);
      var msg = "ccus=" + veda.ticket;
      socket.send(msg); //Handshake
      //console.log("client -> server:", msg);
      var uris = Object.keys(list);
      self.synchronize();
      uris.map(self.subscribe);
      self.trigger("on");
    }

    function closedHandler(event) {
      //console.log("client: socket closed", event);
      self.trigger("off");
      if (!connectInterval) {
        connectInterval = setInterval(function () {
          socket = initSocket();
        }, connectDelay);
      }
    }

    function messageHandler(event) {
      var msg = event.data,
          uris;
      //console.log("server -> client:", msg);
      switch ( true ) {
        case ( msg.indexOf("=") === 0 ):
          // Synchronize subscription
          uris = msg.substr(1);
          if (!uris) { return }
          uris = uris.split(",");
          for (var i = 0; i < uris.length; i++) {
            var tmp = uris[i].split("="),
                uri = tmp[0],
                updateCounter = parseInt(tmp[1]),
                list = self.list();
            list[uri] = list[uri] ? {
              subscribeCounter: list[uri].subscribeCounter,
              updateCounter: updateCounter
            } : {
              subscribeCounter: 1,
              updateCounter: updateCounter
            };
          }
        break;
        default:
          // Update individuals
          uris = msg.split(",");
          for (var i = 0; i < uris.length; i++) {
            try {
              var tmp = uris[i].split("="),
                  uri = tmp[0],
                  updateCounter = parseInt(tmp[1]),
                  individual = new veda.IndividualModel(uri),
                  list = self.list();
              list[uri] = list[uri] ? {
                subscribeCounter: list[uri].subscribeCounter,
                updateCounter: updateCounter
              } : {
                subscribeCounter: 1,
                updateCounter: updateCounter
              };
              if ( !individual.hasValue("v-s:updateCounter") || individual["v-s:updateCounter"][0] !== updateCounter ) {
                individual.reset();
              }
            } catch (e) {
              console.log("error: individual update service failed for id =", uri, e);
            }
          }
        break;
      }
    }

  }

});
