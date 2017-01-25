// Notification module

veda.Module(function Notify(veda) { "use strict";

  veda.Notify = function () {
    // Singleton pattern
    if (veda.Notify.prototype._singletonInstance) {
      return veda.Notify.prototype._singletonInstance;
    }
    // Notify function
    var notificationContainer = $("#notification-container");
    var notificationTmpl = $("#notification-template").html();
    function notify(type, msg) {
      var notification = $(notificationTmpl).addClass("alert-" + type).prependTo(notificationContainer),
          durationFade = 200,
          durationShown = 10000,
          status = msg.status,
          description = msg.description && msg.description.length > 70 ? msg.description.substring(0, 70) + "..." : msg.description,
          iconClass = "fa fa-lg fa-" + (
            type === "danger" ? "times-circle" :
            type === "info" ? "info-circle" :
            type === "success" ? "check-circle" :
            type === "warning" ? "exclamation-circle" : ""
          ),
          timeout = setTimeout(function () {
            notification.hide(durationFade, function () { $(this).remove(); });
          }, durationShown);
      notification.find(".msg-icon").addClass( iconClass );
      notification.find(".msg-status").text( status );
      notification.find(".msg-description").text( description );
      notification.one("remove", function () { clearTimeout(timeout); });
      notification.show(durationFade);
      notificationContainer
        .children()
        .slice(3)
        .hide(durationFade, function () { $(this).remove() });
    }
    return veda.Notify.prototype._singletonInstance = notify;
  }

  // Errors & notifications
  var notify = new veda.Notify();

  veda.on("danger info warning success", function (type, msg) {
    console.log ? console.log( (new Date()).toISOString(), type, ":", JSON.stringify(msg) ) : null;
    switch (msg.status) {
      case 0:
        serverWatch();
        break;
      case 422:
      case 472:
        break;
      case 400:
      case 403:
      case 404:
      case 429:
      case 470:
      case 471:
      case 473:
      case 474:
      case 475:
      case 476:
      case 477:
      case 500:
      case 501:
      case 503:
      case 904:
      case 1021:
      case 1022:
      case 1118:
      case 4000:
        notify(type, msg);
        break;
      default:
        notify(type, msg);
    }
  });

  // Check server health after crash
  var interval;
  function serverWatch() {
    if (interval) { return; }
    var duration = 10000;
    veda.trigger("danger", {status: "Связь потеряна"});
    interval = setInterval(function () {
      try {
        get_individual(veda.ticket, "cfg:OntoVsn");
        clearInterval(interval);
        interval = undefined;
        veda.trigger("success", {status: "Связь восстановлена"});
      } catch (ex) {
        veda.trigger("danger", {status: "Связь потеряна"});
      }
    }, duration);
  }

});
