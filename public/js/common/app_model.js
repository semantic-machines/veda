// Veda application Model

;(function (veda) { "use strict";

	veda.AppModel = function (config) {

		var self = riot.observable(this);
		
		self.config = config;
		self.user_uri = self.ticket = self.end_time = "";
		self.cache = {};
		self.ontology = {};

		// Define Model functions
		self.login = function (username, password) {
			var res = authenticate(username, password);
			self.ticket = res.id;
			if (!self.ticket) return;
			self.user_uri = res.user_uri;
			self.end_time = Math.floor((res.end_time - 621355968000000000) / 10000 );
			return {
				ticket: self.ticket, 
				user_uri: self.user_uri, 
				end_time: self.end_time
			};
		};
		
		self.logout = function() {
			self.user_uri = self.ticket = self.end_time = "";
			self.cache = {};
			self.ontology = {};
			//self.trigger("login:failed");
		};
		
		self.load = function (page, params) {
			switch (page) {
				case "console":
					veda.Util.construct(veda.ConsoleModel, params);
					break;
				case "search":
					veda.Util.construct(veda.SearchModel, params);
					break;
				case "graph":
					self.trigger.apply(self, ["load:graph"].concat(params));
					break;
				default:
					if (!params[0]) { params[0] = "#main"; }
					veda.Util.construct(veda.IndividualModel, [page].concat(params));
			}
		};
		
		// Load ontology
		self.init = function () {
			self.ontology = new veda.OntologyModel();
			self.user = new veda.UserModel(self.user_uri);
			self.trigger("started");
		};
		
		self.on("error", function (error) {
			switch (error.status) {
				case 0:
					alert("Операция не выполнена. Сервер недоступен. Пожалуйста, оставайтесь на этой странице и обратитесь в службу тех. поддержки. / Operation failed. Server is unavailable. Please keep this page open and call support team. \n\n Error: "+JSON.stringify(error));
					console.log ? console.log("Error:", JSON.stringify(error)) : null;
					break;
				case 422:
				case 472:
					console.log ? console.log("Error:", JSON.stringify(error)) : null;
					break;
				case 429:
					alert("Операция не выполнена. Данные не сохранены. Пожалуйста,  оставайтесь на этой странице и обратитесь в службу тех. поддержки. / Operation failed. Data wasn't saved. Please keep this page open and call support team. \n\n Error: "+JSON.stringify(error));
					console.log ? console.log("Error:", JSON.stringify(error)) : null;
					break;
				case 471: 
					self.logout(); 
					break;
				default: 
					alert("Операция не выполнена. Пожалуйста, оставайтесь на этой странице и обратитесь в службу тех. поддержки. / Operation failed. Please keep this page open and call support team. \n\n Error: "+JSON.stringify(error));
					console.log ? console.log("Error:", JSON.stringify(error)) : null;
			}
		});
		
		return self;
	};
	
})(veda);
