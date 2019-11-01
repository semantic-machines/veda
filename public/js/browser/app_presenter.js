// Veda application presenter

veda.Module(function (veda) { "use strict";

  // View resource using special templates:
  // "v-ui:ttl" on Ctrl + Alt + Click
  // "v-ui:json" on Alt + Shift + Click
  // "v-ui:generic" on Ctrl + Alt + Shift + Click
  $("body").on("click", "[resource], [about]", function (e) {
    var uri = $(this).attr("resource") || $(this).attr("about");
    var hash = "#/" + uri;
    if (e.altKey && e.ctrlKey && e.shiftKey) {
      e.preventDefault();
      e.stopPropagation();
      setTimeout(function () {
        riot.route(hash +  "//v-ui:generic");
      });
    } else if (e.altKey && e.ctrlKey) {
      e.preventDefault();
      e.stopPropagation();
      setTimeout(function () {
        riot.route(hash +  "//v-ui:ttl");
      });
    } else if (e.altKey && e.shiftKey) {
      e.preventDefault();
      e.stopPropagation();
      setTimeout(function () {
        riot.route(hash +  "//v-ui:json");
      });
    }
  });
  // Outline resource containers to switch view to special templates
  var outlined = [];
  $(document)
    .on("keydown", function (e) {
      if (e.altKey && e.shiftKey || e.altKey && e.ctrlKey || e.altKey && e.ctrlKey && e.shiftKey) {
        $("body").on("mouseover", "[resource], [about]", outline);
      }
    })
    .on("keyup", removeOutline);
  function outline(e) {
    if (e.altKey && e.shiftKey || e.altKey && e.ctrlKey || e.altKey && e.ctrlKey && e.shiftKey) {
      e.stopPropagation();
      outlined.forEach(function (item) { item.removeAttr("title").removeClass("gray-outline") });
      var $this = $(this);
      $this.addClass("gray-outline").attr("title", $this.attr("resource") || $this.attr("about"));
      outlined = [ $this ];
    } else {
      removeOutline(e);
    }
  }
  function removeOutline(e) {
    $("body").off("mouseover", outline);
    outlined.forEach(function (item) { item.removeAttr("title").removeClass("gray-outline") });
    outlined = [];
  }

  // Localize resources on language change
  veda.on("language:changed", function () {
    var resourcesNodes = $("[resource], [about]");
    var resources = resourcesNodes.map(function () {
      var $this = $(this);
      return $this.attr("about") || $this.attr("resource");
    }).get();
    resources = veda.Util.unique(resources);
    resources.forEach(function (resource_uri) {
      var resource = new veda.IndividualModel(resource_uri);
      for (var property_uri in resource.properties) {
        if (property_uri === "@") { continue; }
        if ( resource.properties[property_uri] && resource.properties[property_uri].length && resource.properties[property_uri][0].type === "String" ) {
          resource.trigger("propertyModified", property_uri, resource.get(property_uri));
          resource.trigger(property_uri, resource.get(property_uri));
        }
      }
    });
  });

  // Prevent empty links routing
  $("body").on("click", "[href='']", function (e) {
    e.preventDefault();
  });

  // Route on link click (IE mandatory!)
  $("body").on("click", "[href^='#/']", function (e) {
    e.preventDefault();
    var hash = $(this).attr("href");
    return ( hash === location.hash ? false : riot.route(hash) );
  });

  // Triggered in veda.start()
  veda.on("language:changed", function () {
    var uris = [];
    $("#app [resource], #app [about]").each(function () {
      var $this = $(this);
      var uri = $this.attr("resource") || $this.attr("about");
      uris.push(uri);
    });
    var unique = veda.Util.unique(uris);
    unique.forEach(localize);

    function localize (uri) {
      var individual = new veda.IndividualModel(uri);
      for (var property_uri in individual.properties) {
        if (property_uri === "@") { continue; }
        if ( individual.hasValue(property_uri) && individual.properties[property_uri][0].type === "String" ) {
          individual.trigger("propertyModified", property_uri, individual.get(property_uri));
          individual.trigger(property_uri, individual.get(property_uri));
        }
      }
    }
  });

  // App loading indicator
  var loadIndicator = $("#load-indicator");
  veda.on("starting", function () {
    loadIndicator.show();
  }).on("started", function () {
    loadIndicator.hide();
  });

  // Triggered in veda.start()
  veda.on("started", function () {
    var layout_param_uri = veda.user.hasValue("v-s:origin", "ExternalUser") ? "cfg:LayoutExternal" : "cfg:Layout" ;
    var layout_param = new veda.IndividualModel( layout_param_uri );
    var welcome_param_uri = veda.user.hasValue("v-s:origin", "ExternalUser") ? "cfg:MainExternal" : "cfg:Main" ;
    var welcome_param = new veda.IndividualModel( welcome_param_uri );

    layout_param.load()

    .then(function (layout_param) {
      return layout_param["rdf:value"][0].load();
    })

    .then(function (layout) {
      return layout.present("#app");
    })

    .then(function () {
      return welcome_param.load();
    })

    .then(function (welcome_param) {
      return welcome_param["rdf:value"][0].load();
    })

    .then(function (welcome) {
      // Router function
      riot.route( function (hash) {
        if ( !hash ) { return welcome.present("#main"); }
        if ( hash.indexOf("#/") < 0 ) { return; }
        var tokens = decodeURI(hash).slice(2).split("/"),
            uri = tokens[0],
            container = tokens[1] || "#main",
            template = tokens[2],
            mode = tokens[3],
            extra = tokens[4];
        if (extra) {
          extra = extra.split("&").reduce(function (acc, pair) {
            var split = pair.split("="),
                name  = split[0] || "",
                values = split[1].split("|") || "";
            acc[name] = acc[name] || [];
            values.forEach(function (value) {
              acc[name].push( parse(value) );
            });
            return acc;
          }, {});
        }

        if (uri) {
          loadIndicator.show();
          var individual = new veda.IndividualModel(uri);
          individual.present(container, template, mode, extra).then(function () {
            loadIndicator.hide();
            if ( !individual.scroll ) {
              window.scrollTo(0, 0);
            }
          });
        } else {
          riot.route("#/" + welcome.id);
        }
      });
      riot.route(location.hash);
    })

    .catch( function (error) {
      var notify = new veda.Notify();
      notify("danger", error);
    });

  });
  function parse (value) {
    if ( !isNaN( value.split(" ").join("").split(",").join(".") ) ) {
      return parseFloat( value.split(" ").join("").split(",").join(".") );
    } else if ( !isNaN( Date.parse(value) ) ) {
      return new Date(value);
    } else if ( value === "true" ) {
      return true;
    } else if ( value === "false" ) {
      return false;
    } else {
      var individ = new veda.IndividualModel(value);
      if ( individ.isSync() && !individ.isNew() ) { return individ; }
    }
    return value || null;
  }

  // Listen to client notifications
  veda.on("started", function () {
    var clientNotification = new veda.IndividualModel("cfg:ClientNotification");
    clientNotification.load().then(function (clientNotification) {
      clientNotification.on("afterReset", checkNotification);
      checkNotification.call(clientNotification);
    });
    function checkNotification() {
      var clientNotification = this;
      var browserNotificationList;
      try {
        browserNotificationList = JSON.parse(localStorage.clientNotification);
      } catch (error) {
        browserNotificationList = [];
      }
      var serverNotificationList = clientNotification.get("rdf:value").map(function (item) { return item.id; });
      if ( !veda.Util.areEqual(browserNotificationList, serverNotificationList) && serverNotificationList.length ) {
        serverNotificationList.reduce(function (p, notification_uri, i) {
          return p.then(function () {
            if (browserNotificationList.indexOf(notification_uri) >= 0) { return; }
            var notification = new veda.IndividualModel(notification_uri);
            return notification.load().then(function (notification) {
              return notification.get("v-s:newsAudience").map(function (audience) {
                return audience.id;
              });
            }).then(function (audience) {
              audience = audience.sort();
              return veda.user.memberOf().then(function (memberOf) {
                memberOf = memberOf.sort();
                var i = 0, j = 0, audience_uri, memberOf_uri;
                while( (audience_uri = audience[i]) && (memberOf_uri = memberOf[j]) ) {
                  if (memberOf_uri < audience_uri) {
                    j++;
                  } else if (memberOf_uri > audience_uri) {
                    i++;
                  } else {
                    return veda.Util.confirm(notification).then(function (confirmed) {
                      if ( confirmed ) {
                        localStorage.clientNotification = JSON.stringify(serverNotificationList);
                        if (notification.hasValue("v-s:script")) {
                          var script = notification.get("v-s:script")[0].toString();
                          eval(script);
                        }
                      }
                    });
                  }
                }
              });
            });
          });
        }, Promise.resolve());
      } else {
        localStorage.clientNotification = JSON.stringify(serverNotificationList);
      }
    }

  });

  // Service worker
  if ("serviceWorker" in navigator) {

    // Install SW
    navigator.serviceWorker.register("/sw.js", { scope: "/" }).then(function(worker) {
    //~ navigator.serviceWorker.register("/sw-simple-offline.js", { scope: "/" }).then(function(worker) {
      console.log("Service worker registered:", worker.scope);
    }).catch(function(error) {
      console.log("Registration failed with " + error);
    });

    // On/off-line handler
    var lineHandler = function (status) {
      var lineStatus = document.getElementById("line-status");
      if ( typeof status === "undefined" || status instanceof Event ) {
        veda.Backend.check().then(function () {
          lineStatus.classList.add("online");
          lineStatus.classList.remove("offline");
          if (navigator.serviceWorker.controller) { navigator.serviceWorker.controller.postMessage("online"); }
        }).catch(function () {
          lineStatus.classList.remove("online");
          lineStatus.classList.add("offline");
          if (navigator.serviceWorker.controller) { navigator.serviceWorker.controller.postMessage("offline"); }
        });
      } else if ( status === "online" ) {
        lineStatus.classList.add("online");
        lineStatus.classList.remove("offline");
        if (navigator.serviceWorker.controller) { navigator.serviceWorker.controller.postMessage("online"); }
      } else {
        lineStatus.classList.remove("online");
        lineStatus.classList.add("offline");
        if (navigator.serviceWorker.controller) { navigator.serviceWorker.controller.postMessage("offline"); }
      }
    }
    window.addEventListener("online", lineHandler);
    window.addEventListener("offline", lineHandler);
    veda.on("online offline", lineHandler);
    lineHandler();

    // Install application prompt
    var showAddToHomeScreen = function () {
      var installApp = document.getElementById("install-app");
      var installBtn = document.getElementById("install-btn");
      var rejectInstallBtn = document.getElementById("reject-install-btn");
      installApp.style.display = "block";
      installBtn.addEventListener("click", addToHomeScreen);
      rejectInstallBtn.addEventListener("click", rejectInstall);
    }
    var addToHomeScreen = function () {
      var installApp = document.getElementById("install-app");
      installApp.style.display = "none";  // Hide the prompt
      deferredPrompt.prompt();  // Wait for the user to respond to the prompt
      deferredPrompt.userChoice
        .then(function(choiceResult) {
          if (choiceResult.outcome === "accepted") {
            console.log("User accepted install prompt");
          } else {
            console.log("User dismissed install prompt");
          }
          deferredPrompt = null;
        });
    }
    var rejectInstall = function () {
      var installApp = document.getElementById("install-app");
      installApp.style.display = "none";
      localStorage.rejectedInstall = true;
    }
    var deferredPrompt;
    window.addEventListener("beforeinstallprompt", function (e) {
      // Prevent Chrome 67 and earlier from automatically showing the prompt
      e.preventDefault();
      // Stash the event so it can be triggered later.
      deferredPrompt = e;
      if ( !localStorage.rejectedInstall ) {
        showAddToHomeScreen();
      }
    });
  }
});
