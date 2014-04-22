
// The ability to split your single-page application (SPA) into loosely-coupled modules

var app;

var Veda = riot.observable(function(arg) {

  // veda() --> return instance
  if (!arg) return app;

  // veda(fn) --> add a new module
  if ($.isFunction(arg)) {
    Veda.on("ready", arg);

  // veda(conf) --> initialize the application
  } else {

    app = new VedaModel(arg);
    
    RegisterModule(app, app);

    app.on("ready", function() {
    	Veda.trigger("ready", app);
    });

    app.trigger("ready");

  }

});
