
// The ability to split your single-page application (SPA) into loosely-coupled modules

var instance;

var veda = riot.observable(function(arg) {

  // veda() --> return instance
  if (!arg) return instance;

  // veda(fn) --> add a new module
  if ($.isFunction(arg)) {
    top.admin.on("ready", arg);

  // veda(conf) --> initialize the application
  } else {

    instance = new Veda(arg);

    instance.on("ready", function() {
      top.veda.trigger("ready", instance);
    });

  }

});

