// Veda application Presenter

veda.Module(function AppPresenter(veda) { "use strict";

  var storage = typeof localStorage !== "undefined" ? localStorage : {
    clear: function () {
      var self = this;
      Object.keys(this).map(function (key) {
        if (typeof self[key] !== "function") delete self[key];
      });
    }
  }

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
      riot.route( function (hash) {
        if ( !hash ) { return welcome.present("#main"); }
        if ( hash.indexOf("#/") < 0 ) { return; }
        var hash_tokens = decodeURIComponent(hash.slice(2)).split("/");
        var uri = hash_tokens[0];
        var params = hash_tokens.slice(1);
        if (!params[0]) { params[0] = "#main"; }
        var individual = uri ? new veda.IndividualModel(uri) : welcome ;
        individual.present.apply(individual, params);
      });
      riot.route(location.hash);
    })

    .catch( function (err) {
      var notify = new veda.Notify();
      notify("danger", err);
    });

  });

  // Login invitation
  var loginTmpl = $("#login-template").html();
  var loginForm = $(loginTmpl);
  var credentials = $(".credentials");
  credentials.append(loginForm);
  var errorMsg = $("#login-error", loginForm);
  var submit = $("#submit", loginForm);
  submit.click( function (e) {
    e.preventDefault();
    var login = $("#login", loginForm).val(),
      password = $("#password", loginForm).val(),
      hash = Sha256.hash(password),
      authResult;

    // Try internal authentication
    veda.login(login, hash)
      // Try ntlm authentication
      .catch(function (error) {
        console.log(error);
        if (ntlm) {
          var params = {
            type: "POST",
            url: ntlm + "ad/",
            data: {
              "login": login,
              "password": password
            },
            async: true
          };
          return $.ajax(params);
        } else {
          throw new Error("No ntlm");
        }
      })
      .then(function (authResult) {
        errorMsg.addClass("hidden");
        veda.trigger("login:success", authResult);
      })
      .catch(function (error) {
        console.log(error);
        errorMsg.removeClass("hidden");
        veda.trigger("login:failed");
      });
  });

  // NTLM auth using iframe
  var ntlm;
  var iframe = $("<iframe>", {"class": "hidden"});
  var ntlmProvider = new veda.IndividualModel("cfg:NTLMAuthProvider", true, false);
  ntlmProvider.load().then(function (ntlmProvider) {
    ntlm = !ntlmProvider.hasValue("v-s:deleted", true) && ntlmProvider.hasValue("rdf:value") && ntlmProvider.get("rdf:value")[0];
    if (ntlm) {
      iframe.appendTo(credentials);
    }
  });

  veda.on("login:failed", function () {
    $("#app").empty();
    delete storage.ticket;
    delete storage.user_uri;
    delete storage.end_time;
    delCookie("ticket");
    if ( ntlm ) {
      iframe.one("load", function () {
        try {
          credentials.addClass("hidden");
          var body = iframe.contents().find("body"),
            ticket = $("#ticket", body).text(),
            user_uri = $("#user_uri", body).text(),
            end_time = $("#end_time", body).text(),
            authResult = {
              ticket: ticket,
              user_uri: user_uri,
              end_time: end_time
            };
          if (ticket && user_uri && end_time) {
            veda.trigger("login:success", authResult);
          } else {
            throw "Not authenticated";
          }
        } catch (ex) {
          console.log(ex);
          credentials.removeClass("hidden");
        }
      });
      document.domain = document.domain;
      iframe.attr("src", ntlm);
    } else {
      credentials.removeClass("hidden");
    }
  });

  // Initialize application if ticket is valid
  veda.on("login:success", function (authResult) {
    credentials.addClass("hidden");
    veda.user_uri = authResult.user_uri;
    veda.ticket = authResult.ticket;
    veda.end_time = authResult.end_time;
    storage.ticket = authResult.ticket;
    storage.user_uri = authResult.user_uri;
    storage.end_time = authResult.end_time.toString();
    setCookie("ticket", authResult.ticket);
    veda.start();

    // Re-login on ticket expiration
    var ticketDelay = new Date( parseInt(veda.end_time) ) - new Date();
    //var ticketDelay = 10000;
    console.log("Ticket will expire in %s hrs.", (ticketDelay / 1000 / 60 / 60).toFixed(2) );
    setTimeout(function () {
      console.log("Ticket expired, re-login.");
      veda.trigger("login:failed");
    }, ticketDelay);
  });

  // Logout handler
  veda.on("logout", function () {
    $("#app").empty();
    delete storage.ticket;
    delete storage.user_uri;
    delete storage.end_time;
    delCookie("ticket");
    credentials.removeClass("hidden");
  });

  // Init application
  veda.init().then(function () {
    // Check if ticket in cookies is valid
    var ticket = storage.ticket,
        user_uri = storage.user_uri,
        end_time = ( new Date() < new Date(parseInt(storage.end_time)) ) && storage.end_time;
    if (ticket && user_uri && end_time) {
      return veda.Backend.is_ticket_valid(ticket);
    } else {
      veda.trigger("login:failed");
      throw new Error("Auth expired");
    }
  }).then(function (valid) {
    if (valid) {
      veda.trigger("login:success", {
        ticket: storage.ticket,
        user_uri: storage.user_uri,
        end_time: storage.end_time
      });
    } else {
      veda.trigger("login:failed");
      throw new Error("Auth expired");
    }
  }).catch(function (error) {
    console.log( new Error("Auth expired") );
    veda.trigger("login:failed");
  });

});
