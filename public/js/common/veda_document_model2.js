// veda_document Model using Module
new Module({
	name: "document2",
	load: function(uri) {
		this.individual = get_individual(app.ticket, uri);
		this.trigger("load");
	},
	save: function() {
		put_individual(app.ticket, this.individual);
	},
	init: function() {
		this.on("load", function() { alert("load triggered")});
	}
}, app);