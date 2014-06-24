// (KarpovR) Derived from riot-admin
// The ability to split your single-page application (SPA) into loosely-coupled modules

var app;

var Veda = riot.observable(function(arg) {

	// Veda() --> return instance
	if (!arg) return app;

	// Veda(fn) --> add a new module
	if (typeof arg == 'function') {

		Veda.on("ready", arg);

	// Veda(conf) --> initialize the application
	} else {

		app = new AppModel(arg);
		
		RegisterModule(app, app);
		
		app.on("ready", function() {
			Veda.trigger("ready", app);
		});

		app.trigger("ready");

	}

});
