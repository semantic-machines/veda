
// The ability to split your single-page application (SPA) into loosely-coupled modules

var app;

var veda = riot.observable(function(arg) {

  // veda() --> return instance
  if (!arg) return app;

  // veda(fn) --> add a new module
  if ($.isFunction(arg)) {
    veda.on("ready", arg);

  // veda(conf) --> initialize the application
  } else {

    app = new VedaModel(arg);
    
    app.RegisterModule(app, app);

    app.on("ready", function() {
      veda.trigger("ready", app);
    });

    app.trigger("ready");

  }

});
