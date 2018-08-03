// Veda application Presenter

veda.Module(function (veda) { "use strict";

  var storage = typeof localStorage !== "undefined" ? localStorage : {
    clear: function () {
      var self = this;
      Object.keys(this).map(function (key) {
        if (typeof self[key] !== "function") delete self[key];
      });
    }
  };

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
  veda.one("started", function () {
    var welcome;
    if (veda.user.hasValue("v-s:origin", "External User")) {
      welcome = (new veda.IndividualModel("cfg:WelcomeExternal"))["rdf:value"][0];
    } else {
      welcome = (new veda.IndividualModel("cfg:Welcome"))["rdf:value"][0];
    }
    // Router function
    riot.route( function (hash) {
      if ( !hash ) {
        return riot.route("#/" + welcome.id);
      }
      if ( hash.indexOf("#/") < 0 ) { return; }
      var tokens = decodeURI(hash).slice(2).split("/"),
          uri = tokens[0],
          container = tokens[1],
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

  veda.on("started", function () {
    var layout;
    if (veda.user.hasValue("v-s:origin", "External User")) {
      layout = (new veda.IndividualModel("cfg:LayoutExternal"))["rdf:value"][0];
    } else {
      layout = (new veda.IndividualModel("cfg:Layout"))["rdf:value"][0];
    }
    layout.present("#app");
    riot.route(location.hash);
  });

  // Login invitation
  var loginForm = $(".login-form");
  var enterLoginPassword = $("#enter-login-password", loginForm);
  var enterNewPassword = $("#enter-new-password", loginForm);
  var loginFailedError = $("#login-failed-error", loginForm);
  var passwordExpiredError = $("#password-expired-error", loginForm);
  var newPasswordError = $("#password-expired-error", loginForm);

  $("#submit-login-password", loginForm).click( function (e) {
    e.preventDefault();
    loginFailedError.addClass("hidden");
    passwordExpiredError.addClass("hidden");
    var login = $("#login", loginForm).val(),
      password = $("#password", loginForm).val(),
      hash = Sha256.hash(password),
      authResult;
    try {
      authResult = veda.login(login, hash);
      //var err = new Error(); err.code = 469; throw err;
    } catch (err) {
      console.log(err);
      authResult = err.code;
      if (ntlm) {
        var params = {
          type: "POST",
          url: ntlm + "ad/",
          data: {
            "login": login,
            "password": password
          },
          async: false
        };
        try {
          authResult = $.ajax(params);
          authResult = JSON.parse( authResult.responseText );
        } catch (err) {
          console.log(err);
          authResult = err.code;
        }
      }
    } finally {
      if (typeof authResult === "object") {
        enterNewPassword.addClass("hidden");
        passwordExpiredError.addClass("hidden");
        enterLoginPassword.removeClass("hidden");
        loginFailedError.addClass("hidden");
        veda.trigger("login:success", authResult);
      } else if ( authResult === 473 ) {
        enterLoginPassword.removeClass("hidden");
        loginFailedError.removeClass("hidden");
        enterNewPassword.addClass("hidden");
        passwordExpiredError.addClass("hidden");
      } else if ( authResult === 469 ) {
        enterNewPassword.removeClass("hidden");
        passwordExpiredError.removeClass("hidden");
        enterLoginPassword.addClass("hidden");
        loginFailedError.addClass("hidden");
      }
    }
  });

  $("#submit-new-password", loginForm).click( function (e) {
    e.preventDefault();
    loginFailedError.addClass("hidden");
    passwordExpiredError.addClass("hidden");
    newPasswordError.addClass("hidden");
    var login = $("#login", loginForm).val(),
      password = $("#new-password", loginForm).val(),
      secret = $("#secret", loginForm).val(),
      hash = Sha256.hash(password),
      authResult;
    try {
      authResult = veda.login(login, hash, secret);
      //authResult = veda.login(login, hash);
    } catch (err) {
      console.log(err);
      newPasswordError.removeClass("hidden");
    } finally {
      if (typeof authResult === "object") {
        enterLoginPassword.removeClass("hidden");
        enterNewPassword.addClass("hidden");
        loginFailedError.addClass("hidden");
        passwordExpiredError.addClass("hidden");
        newPasswordError.addClass("hidden");
        veda.trigger("login:success", authResult);
      }
    }
  });

  // NTLM auth using iframe
  var ntlmProvider = new veda.IndividualModel({uri: "cfg:NTLMAuthProvider", cache: true, init: false}),
    ntlm = !ntlmProvider.hasValue("v-s:deleted", true) && ntlmProvider.hasValue("rdf:value") && ntlmProvider.get("rdf:value")[0],
    iframe;
  if ( ntlm ) {
    iframe = $("<iframe>", {"class": "hidden"}).appendTo(loginForm);
  }

  veda.on("login:failed", function () {
    $("#app").empty();
    delete storage.ticket;
    delete storage.user_uri;
    delete storage.end_time;
    veda.Util.delCookie("ticket");
    if ( ntlm ) {
      iframe.one("load", function () {
        try {
          loginForm.addClass("hidden");
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
          loginForm.removeClass("hidden");
        }
      });
      document.domain = document.domain;
      iframe.attr("src", ntlm);
    } else {
      loginForm.removeClass("hidden");
    }
  });

  // Initialize application if ticket is valid
  veda.on("login:success", function (authResult) {
    loginForm.addClass("hidden");
    veda.user_uri = authResult.user_uri;
    veda.ticket = authResult.ticket;
    veda.end_time = authResult.end_time;
    storage.ticket = authResult.ticket;
    storage.user_uri = authResult.user_uri;
    storage.end_time = authResult.end_time.toString();
    // Re-login on ticket expiration
    var ticketDelay = new Date( parseInt(veda.end_time) ) - new Date();
    veda.Util.setCookie("ticket", authResult.ticket, { path:"/files" });
    console.log("Ticket will expire in %s hrs.", (ticketDelay / 1000 / 60 / 60).toFixed(2) );
    setTimeout(function () {
      console.log("Ticket expired, re-login.");
      veda.trigger("login:failed");
    }, ticketDelay);
    veda.start();
  });

  // Logout handler
  veda.on("logout", function () {
    $("#app").empty();
    delete storage.ticket;
    delete storage.user_uri;
    delete storage.end_time;
    veda.Util.delCookie("ticket");
    loginForm.removeClass("hidden");
  });

  // Load ontology
  veda.init();

  try {
    // Check if ticket in storage is valid
    var ticket = storage.ticket,
        user_uri = storage.user_uri,
        end_time = storage.end_time && ( new Date() < new Date(parseInt(storage.end_time)) ) ? storage.end_time : undefined ;

    if ( ticket && user_uri && end_time && is_ticket_valid(ticket) ) {
      veda.trigger("login:success", {
        ticket: ticket,
        user_uri: user_uri,
        end_time: end_time
      });
    } else {
      veda.trigger("login:failed");
    }
  } catch (ex) {
    console.log(ex);
    veda.trigger("login:failed");
  }

});
