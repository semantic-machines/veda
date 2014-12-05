// (KarpovR) Derived from riot-admin
// The ability to split single-page application (SPA) into loosely-coupled modules

var veda;

var Veda = riot.observable(function(arg) {

	// Veda() --> return instance
	if (!arg) return veda;

	// Veda(fn) --> add a new module
	if (typeof arg == 'function') {

		Veda.on("ready", arg);

	// Veda(conf) --> initialize the application
	} else {

		veda = new AppModel(arg);
		
		RegisterModule(veda, veda);
		
		veda.on("ready", function() {
			Veda.trigger("ready", veda);
		});

		veda.trigger("ready");

	}

});
