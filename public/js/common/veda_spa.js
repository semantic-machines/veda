// The ability to split your single-page application (SPA) into loosely-coupled modules

var app;

var veda = $.observable(function(arg) {

  // veda() --> return instance
  if (!arg) return instance;

  // veda(fn) --> add a new module
  if ($.isFunction(fn)) {
    veda.on("ready", fn);

  // veda(page) --> initialize the application
  } else {

    app = new Veda(arg);

    app.on("ready", function() {
      veda.trigger("ready", app);
    });

  }

});

