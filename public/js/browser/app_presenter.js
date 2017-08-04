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

  // Triggered in veda.init()
  veda.one("started", function () {
    var welcome;
    if (veda.user.hasValue("v-s:origin", "External User")) {
      welcome = (new veda.IndividualModel("cfg:WelcomeExternal"))["rdf:value"][0];
    } else {
      welcome = (new veda.IndividualModel("cfg:Welcome"))["rdf:value"][0];
    }
    // Router function
    riot.route( function (hash) {
      if ( !hash ) { return welcome.present("#main"); }
      if ( hash.indexOf("#/") < 0 ) { return; }
      var hash_tokens = decodeURIComponent(hash).slice(2).split("/");
      var page = hash_tokens[0];
      var params = hash_tokens.slice(1);
      page ? veda.load(page, params) : welcome.present("#main");
    });
  });
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
  var loginTmpl = $("#login-template").html();
  var loginContainer = $("#login-container");
  loginContainer.html(loginTmpl);
  var errorMsg = $("#login-error", loginContainer);
  var submit = $("#submit", loginContainer);
  submit.click( function (e) {
    e.preventDefault();
    var login = $("#login", loginContainer).val(),
      password = $("#password", loginContainer).val(),
      hash = Sha256.hash(password),
      authResult;
    try {
      authResult = veda.login(login, hash);
    } catch (ex1) {
      console.log(ex1);
      authResult = undefined;
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
        } catch (ex2) {
          console.log(ex2);
          authResult = undefined;
        }
      }
    } finally {
      if (authResult) {
        errorMsg.addClass("hidden");
        veda.trigger("login:success", authResult);
      } else {
        errorMsg.removeClass("hidden");
        veda.trigger("login:failed");
      }
    }
  });

  // NTLM auth using iframe
  var ntlmProvider = new veda.IndividualModel({uri: "cfg:NTLMAuthProvider", cache: true, init: false}),
    ntlm = !ntlmProvider.hasValue("v-s:deleted", true) && ntlmProvider.hasValue("rdf:value") && ntlmProvider.get("rdf:value")[0],
    iframe = $("<iframe>", {"class": "hidden"});
  if ( ntlm ) {
    iframe.appendTo(loginContainer);
  }

  veda.on("login:failed", function () {
    $("#app").empty();
    delete storage.ticket;
    delete storage.user_uri;
    delete storage.end_time;
    delCookie("ticket");
    if ( ntlm ) {
      iframe.one("load", function () {
        try {
          loginContainer.addClass("hidden");
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
          loginContainer.removeClass("hidden");
        }
      });
      document.domain = document.domain;
      iframe.attr("src", ntlm);
    } else {
      loginContainer.removeClass("hidden");
    }
  });

  // Initialize application if ticket is valid
  veda.on("login:success", function (authResult) {
    loginContainer.addClass("hidden");
    veda.user_uri = authResult.user_uri;
    veda.ticket = authResult.ticket;
    veda.end_time = authResult.end_time;
    storage.ticket = authResult.ticket;
    storage.user_uri = authResult.user_uri;
    storage.end_time = authResult.end_time.toString();
    setCookie("ticket", authResult.ticket);
    veda.init();
  });

  // Logout handler
  veda.on("logout", function () {
    $("#app").empty();
    delete storage.ticket;
    delete storage.user_uri;
    delete storage.end_time;
    delCookie("ticket");
    loginContainer.removeClass("hidden");
  });

  try {
    // Check if ticket in cookies is valid
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
