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
      console.log ? console.log( (new Date()).toISOString(), type, ":", JSON.stringify(msg) ) : null;
      if (msg.status === 0) {
        serverWatch();
        return;
      } else if (msg.status === 472 || msg.status === 422) {
        return;
      }
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

  // Check server health after crash
  var interval;
  function serverWatch() {
    if (interval) { return; }
    var duration = 10000;
    notify("danger", {status: "Связь потеряна"});
    interval = setInterval(function () {
      try {
        get_individual(veda.ticket, "cfg:OntoVsn");
        clearInterval(interval);
        interval = undefined;
        notify("success", {status: "Связь восстановлена"});
      } catch (ex) {
        notify("danger", {status: "Связь потеряна"});
      }
    }, duration);
  }

});
