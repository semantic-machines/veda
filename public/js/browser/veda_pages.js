var veda_pages = {
	load: function(path) {

		var els = path.split("/"),
		page = els[0];

		return {
			path: path,
			type: page || "console",
			data: page == "console" ? console : []
		};
	}
};