// (KarpovR) Derived from riot-admin
// The ability to split single-page application (SPA) into loosely-coupled modules

var veda = {};

;(function (veda) { "use strict";
	veda.Present = riot.observable(function(arg) {
		
		// veda.Present(fn) --> add a new presenter
		if (typeof arg == 'function') {

			veda.Present.on("ready", arg);

		// veda.Present(conf) --> initialize the application
		} else {

			veda.AppModel.call(veda, arg);
			
			//veda.RegisterModule(veda, veda);
			
			veda.on("ready", function() {
				veda.Present.trigger("ready", veda);
			});

			veda.trigger("ready");

		}

	});
	
})(veda);
