// Veda application presenter

veda.Module(function (veda) { "use strict";

  // Route to resource ttl view on Ctrl + Alt + Click
  $("body").on("click", "[resource][typeof], [about]", function (e) {
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

  // App loading indicator
  var appLoadIndicator = $("#app-load-indicator");
  veda.on("init:progress", function (progress) {
    if (progress !== 100) {
      appLoadIndicator.removeClass("hidden");
    } else {
      appLoadIndicator.addClass("hidden");
    }
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

  // Triggered in veda.start()
  veda.on("started", function () {
    var layout_param_uri = veda.user.hasValue("v-s:origin", "External User") ? "cfg:LayoutExternal" : "cfg:Layout" ;
    var layout_param = new veda.IndividualModel( layout_param_uri );
    var welcome_param_uri = veda.user.hasValue("v-s:origin", "External User") ? "cfg:WelcomeExternal" : "cfg:Welcome" ;
    var welcome_param = new veda.IndividualModel( welcome_param_uri );

    layout_param.load()

    .then(function (layout_param) {
      return layout_param["rdf:value"][0].load();
    })

    .then(function (layout) {
      layout.present("#app");
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
                value = split[1] || "";
            acc[name] = acc[name] || [];
            acc[name].push( parse(value) );
            return acc;
          }, {});
        }
        if (uri === "drafts") {
          return veda.trigger("load:drafts");
        }
        if (uri) {
          var individual = new veda.IndividualModel(uri);
          individual.present(container, template, mode, extra);
        } else {
          riot.route("#/" + welcome.id);
        }
      });
      riot.route(location.hash);
    })

    .catch( function (err) {
      var notify = new veda.Notify();
      notify("danger", err);
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

});
