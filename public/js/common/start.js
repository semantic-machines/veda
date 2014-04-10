// Veda application
"use strict";
var app = null;
app = Module(new Veda(), app, "veda");
Module(new Console(), app, "console");
Module(new Document(), app, "document");
