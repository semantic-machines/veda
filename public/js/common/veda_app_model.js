// Veda application Model

function Veda() {
	var self = $.observable(this);
	self.user_uri = "";
	self.ticket = "";
	self.end_time = 0;
	self.login = function (username, password) {
		var res = authenticate(username, password);
		self.ticket = res.id;
		self.user_uri = res.user_uri;
		self.end_time = res.end_time;
		self.trigger("login");
	};
	self.logout = function() {
		self.ticket = "";
	};
	self.on("login", function() { 
		Module(new Document(self.user_uri), self, "user");
	});
};

