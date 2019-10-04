// Veda application authentication

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
      hash = Sha256.hash(password);

    // Try internal authentication
    veda.login(login, hash)
      // Try ntlm authentication
      .catch(function (error) {
        console.log(error);
        if (error.code === 429) { throw error; }
        var ntlmProvider = new veda.IndividualModel("cfg:NTLMAuthProvider", true, false);
        return ntlmProvider.load().then(function (ntlmProvider) {
          var ntlm = !ntlmProvider.hasValue("v-s:deleted", true) && ntlmProvider.hasValue("rdf:value") && ntlmProvider.get("rdf:value")[0];
          if (ntlm) {
            var params = {
              type: "POST",
              url: "/ad/",
              data: {
                "login": login,
                "password": password
              },
              dataType: "json",
              async: true
            };
            return $.ajax(params);
          } else {
            throw error;
          }
        });
      })
      .then(handleLoginSuccess)
      .catch(handleLoginError);
  });

  $("#new-password, #confirm-new-password", loginForm).change(validateNewPassword);
  $("#secret", loginForm).keyup(validateNewPassword);
  function validateNewPassword() {
    var re = new RegExp("^(?=.*[a-z])(?=.*[A-Z])(?=.*[0-9])(?=.{6,})");
    var submit = $("#submit-new-password", loginForm);
    var newPasswordGroup = $("#new-password-group", loginForm);
    var newPassword = $("#new-password", loginForm);
    var confirmNewPassword = $("#confirm-new-password", loginForm);
    var passwordStrength = $(".password-strength", loginForm);
    var passwordMustMatch = $(".password-must-match", loginForm);
    var secretGroup = $("#secret-group", loginForm);
    var secret = $("#secret", loginForm);
    var enterSecret = $(".enter-secret", loginForm);

    var reMatch = re.test( newPassword.val() );
    var passwordsMatch = confirmNewPassword.val() === newPassword.val();
    var isSecret = !!secret.val();
    var isValid = reMatch && passwordsMatch && isSecret;

    if ( !reMatch ) {
      passwordStrength.show();
    } else {
      passwordStrength.hide();
    }
    if ( !passwordsMatch ) {
      passwordMustMatch.show();
    } else {
      passwordMustMatch.hide();
    }
    if ( !isSecret ) {
      enterSecret.show();
    } else {
      enterSecret.hide();
    }
    if ( !isValid ) {
      submit.attr("disabled", "disabled");
    } else {
      submit.removeAttr("disabled", "disabled");
    }
  }

  $("#submit-new-password", loginForm).click( function (e) {
    e.preventDefault();
    var login = $("#login", loginForm).val(),
      password = $("#new-password", loginForm).val(),
      secret = $("#secret", loginForm).val(),
      hash = Sha256.hash(password);

    veda.login(login, hash, secret)
      .then(handleLoginSuccess)
      .catch(handleLoginError);
  });

  var forgotPasswordPressed;
  $("#forgot-password, #request-secret", loginForm).click( function (e) {
    e.preventDefault();
    forgotPasswordPressed = true;
    var login = $("#login", loginForm).val(),
      secret = "?";

    veda.login(login, undefined, secret)
      .then(handleLoginSuccess)
      .catch(handleLoginError);
  });

  function handleLoginError(error) {
    var enterLoginPassword = $("#enter-login-password", loginForm).hide();
    var enterNewPassword = $("#enter-new-password", loginForm).hide();
    var loginFailedError = $("#login-failed-error", loginForm).hide();
    var passwordExpiredError = $("#password-expired-error", loginForm).hide();
    var newPasswordError = $("#password-expired-error", loginForm).hide();
    var invalidSecretError = $("#invalid-secret-error", loginForm).hide();
    var invalidPasswordError = $("#invalid-password-error", loginForm).hide();
    var tooManyFailsError = $("#too-many-fails-error", loginForm).hide();
    var secretRequestInfo = $("#secret-request-info", loginForm).hide();
    switch (error.code) {
      case 429: // Too many requests
        enterLoginPassword.show();
        tooManyFailsError.show();
        break;
      case 465: // Empty password
      case 466: // New password is equal to old
      case 467: // Invalid password
        enterNewPassword.show();
        invalidPasswordError.show();
        break;
      case 468: // Invalid secret
        enterNewPassword.show();
        invalidSecretError.show();
        break;
      case 469: // Password expired
        enterNewPassword.show();
        if ( !forgotPasswordPressed ) {
          passwordExpiredError.show();
        }
        secretRequestInfo.show();
        break;
      case 472: // Not authorized
      case 473: // Authentication failed
      default:
        enterLoginPassword.show();
        loginFailedError.show();
        break;
    }
  }

  function handleLoginSuccess(authResult) {
    var enterLoginPassword = $("#enter-login-password", loginForm).show();
    var enterNewPassword = $("#enter-new-password", loginForm).hide();
    var loginFailedError = $("#login-failed-error", loginForm).hide();
    var passwordExpiredError = $("#password-expired-error", loginForm).hide();
    var newPasswordError = $("#password-expired-error", loginForm).hide();
    var invalidSecretError = $("#invalid-secret-error", loginForm).hide();
    var invalidPasswordError = $("#invalid-password-error", loginForm).hide();
    var secretRequestInfo = $("#secret-request-info", loginForm).hide();
    veda.trigger("login:success", authResult);
  }

  veda.on("login:failed", function () {
    $("#app").empty();
    delete storage.ticket;
    delete storage.user_uri;
    delete storage.end_time;
    veda.Util.delCookie("ticket");

    if (storage.logout) {
      loginForm.show();
      delete storage.logout;
      return;
    }

    // NTLM auth using iframe
    var ntlmProvider = new veda.IndividualModel("cfg:NTLMAuthProvider", true, false);
    ntlmProvider.load().then(function (ntlmProvider) {
      var ntlm = !ntlmProvider.hasValue("v-s:deleted", true) && ntlmProvider.hasValue("rdf:value") && ntlmProvider.get("rdf:value")[0];
      if (ntlm) {
        var iframe = $("<iframe>", {"class": "hidden"});
        iframe.appendTo(loginForm);
        iframe.one("load", function () {
          try {
            loginForm.hide();
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
            loginForm.show();
          }
        });
        document.domain = document.domain;
        iframe.attr("src", ntlm);
      } else {
        loginForm.show();
      }
    });
  });

  // Initialize application if ticket is valid
  veda.on("login:success", function (authResult) {
    loginForm.hide();
    veda.user_uri = storage.user_uri = authResult.user_uri;
    veda.ticket = storage.ticket = authResult.ticket;
    veda.end_time = storage.end_time = authResult.end_time;
    veda.Util.setCookie("ticket", authResult.ticket, { path:"/files" });
    // Re-login on ticket expiration
    if( veda.end_time ) {
      var ticketDelay = parseInt(veda.end_time) - Date.now();
      var ticketDelayHours = Math.floor(ticketDelay / 1000 / 60 / 60);
      var ticketDelayMinutes = Math.floor(ticketDelay / 1000 / 60 - ticketDelayHours  * 60);
      console.log("Ticket will expire in %d hrs. %d mins.", ticketDelayHours, ticketDelayMinutes);
      setTimeout(function () {
        console.log("Ticket expired, re-login.");
        veda.trigger("login:failed");
      }, ticketDelay);
    }
    veda.start();
  });

  // Logout handler
  veda.on("logout", function () {
    delete storage.ticket;
    delete storage.user_uri;
    delete storage.end_time;
    veda.Util.delCookie("ticket");
    storage.logout = true;
    location.reload();
  });

  // Init application
  veda.init()
    .then(function () {
      // Check if ticket in cookies is valid
      var ticket = storage.ticket,
          user_uri = storage.user_uri,
          end_time = ( new Date() < new Date(parseInt(storage.end_time)) ) && storage.end_time;
      if (ticket && user_uri && end_time) {
        return veda.Backend.is_ticket_valid(ticket);
      } else {
        return false;
      }
    })
    .then(function (valid) {
      if (valid) {
        veda.trigger("login:success", {
          ticket: storage.ticket,
          user_uri: storage.user_uri,
          end_time: storage.end_time
        });
      } else {
        var authRequired = new veda.IndividualModel("cfg:AuthRequired");
        authRequired.load().then(function (authRequiredParam) {
          if ( authRequiredParam && authRequiredParam.hasValue("rdf:value", false) ) {
            veda.trigger("login:success", {
              ticket: "",
              user_uri: "cfg:Guest",
              end_time: 0
            });
          } else {
            veda.trigger("login:failed");
          }
        });
      }
    });
});
