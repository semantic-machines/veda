// Drafts Model

veda.Module(function (veda) { "use strict";
	
	var storage = typeof localStorage !== "undefined" ? localStorage : {
		clear: function () {
			var self = this;
			Object.keys(this).map(function (key) {
				if (typeof self[key] !== "function") delete self[key];
			});
		}
	}
	
	veda.DraftsModel = function () {
		
		var self = this;

		try { 
			self._ = JSON.parse(storage.drafts);
		} catch (e) {
			self._ = {};
		}

		Object.keys(self._).map(function (key) {
			var draft = self._[key];
			if (draft) self[key] = new veda.IndividualModel( draft );
		});
	};
	
	var proto = veda.DraftsModel.prototype;
	
	proto.get = function (uri) {
		return this[uri];
	};

	proto.set = function (uri, data) {
		this[uri] = data;
		this._drafts[uri] = data.toJson();
		storage.drafts = JSON.stringify(this._drafts);
	};

	proto.remove = function (uri) {
		delete this[uri];
		delete this._[uri];
		storage.drafts = JSON.stringify(this._);
	};

});
