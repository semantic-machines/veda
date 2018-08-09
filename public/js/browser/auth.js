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

  // Login invitation
  var loginForm = $(".login-form");

  $("#submit-login-password", loginForm).click( function (e) {
    e.preventDefault();
    var login = $("#login", loginForm).val(),
      password = $("#password", loginForm).val(),
      hash = Sha256.hash(password),
      authResult;

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
        return veda.trigger("login:success", authResult);
      } catch (error) {
        console.log("manual ntlm auth failed", error);
      }
    }
    try {
      authResult = veda.login(login, hash);
    } catch (error) {
      console.log("veda auth failed", error);
      authResult = error;
    } finally {
      if (authResult instanceof Error) {
        handleLoginError(authResult);
      } else {
        handleLoginSuccess(authResult);
      }
    }
  });

  $("#submit-new-password", loginForm).click( function (e) {
    e.preventDefault();
    var login = $("#login", loginForm).val(),
      password = $("#new-password", loginForm).val(),
      secret = $("#secret", loginForm).val(),
      hash = Sha256.hash(password),
      authResult;
    try {
      authResult = veda.login(login, hash, secret);
    } catch (error) {
      console.log("password change failed", error);
      authResult = error;
    } finally {
      if (authResult instanceof Error) {
        handleLoginError(authResult);
      } else {
        handleLoginSuccess(authResult);
      }
    }
  });

  $("#forgot-password, #request-secret", loginForm).click( function (e) {
    e.preventDefault();
    var login = $("#login", loginForm).val(),
      secret = "?",
      authResult;
    try {
      authResult = veda.login(login, undefined, secret);
    } catch (error) {
      handleLoginError(error);
    }
  });

  function handleLoginError(error) {
    var enterLoginPassword = $("#enter-login-password", loginForm).addClass("hidden");
    var enterNewPassword = $("#enter-new-password", loginForm).addClass("hidden");
    var loginFailedError = $("#login-failed-error", loginForm).addClass("hidden");
    var passwordExpiredError = $("#password-expired-error", loginForm).addClass("hidden");
    var newPasswordError = $("#password-expired-error", loginForm).addClass("hidden");
    var invalidSecretError = $("#invalid-secret-error", loginForm).addClass("hidden");
    var invalidPasswordError = $("#invalid-password-error", loginForm).addClass("hidden");
    var secretRequestInfo = $("#secret-request-info", loginForm).addClass("hidden");
    switch (error.code) {
      case 465: // Empty password
      case 466: // New password is equal to old
      case 467: // Invalid password
        enterNewPassword.removeClass("hidden");
        invalidPasswordError.removeClass("hidden");
        break;
      case 468: // Invalid secret
        enterNewPassword.removeClass("hidden");
        invalidSecretError.removeClass("hidden");
        break;
      case 469: // Password expired
        enterNewPassword.removeClass("hidden");
        passwordExpiredError.removeClass("hidden");
        secretRequestInfo.removeClass("hidden");
        break;
      case 472: // Not authorized
      case 473: // Authentication failed
        enterLoginPassword.removeClass("hidden");
        loginFailedError.removeClass("hidden");
        break;
    }
  }

  function handleLoginSuccess(authResult) {
    var enterLoginPassword = $("#enter-login-password", loginForm).removeClass("hidden");
    var enterNewPassword = $("#enter-new-password", loginForm).addClass("hidden");
    var loginFailedError = $("#login-failed-error", loginForm).addClass("hidden");
    var passwordExpiredError = $("#password-expired-error", loginForm).addClass("hidden");
    var newPasswordError = $("#password-expired-error", loginForm).addClass("hidden");
    var invalidSecretError = $("#invalid-secret-error", loginForm).addClass("hidden");
    var invalidPasswordError = $("#invalid-password-error", loginForm).addClass("hidden");
    var secretRequestInfo = $("#secret-request-info", loginForm).addClass("hidden");
    veda.trigger("login:success", authResult);
  }

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
            throw "auto ntlm auth failed";
          }
        } catch (err) {
          console.log(err);
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
    veda.user_uri = storage.user_uri = authResult.user_uri;
    veda.ticket = storage.ticket = authResult.ticket;
    veda.end_time = storage.end_time = authResult.end_time;
    // Re-login on ticket expiration
    var ticketDelay = parseInt(veda.end_time) - Date.now();
    veda.Util.setCookie("ticket", authResult.ticket, { path:"/files" });
    var ticketDelayHours = Math.floor(ticketDelay / 1000 / 60 / 60);
    var ticketDelayMinutes = Math.floor(ticketDelay / 1000 / 60 - ticketDelayHours  * 60);
    console.log("Ticket will expire in %d hrs. %d mins.", ticketDelayHours, ticketDelayMinutes);
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
