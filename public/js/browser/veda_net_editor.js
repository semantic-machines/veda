/**
 * @class jsWorkflow.Instance
 * 
 * Net editor. Used to create / modify / view workflow nets.
 * 
 * Inspired by [http://github.com/hemantsshetty/jsWorkflow][1]
 * 
 * [1]: http://github.com/hemantsshetty/jsWorkflow
 */
var jsWorkflow = jsWorkflow || {};

// Leveraging the ready function of jsPlumb.
jsWorkflow.ready = jsPlumb.ready;

// Self execute this code
(function() {
	
    // No API call should be made until the DOM has been initialized.
    jsWorkflow.ready(function() {
        /**
         *Create a workflow instance.
         *@constructor Instance
         */
        jsWorkflow.Instance = function() {

            // Get a new instance of jsPlumb.
            this.instance = jsPlumb.getInstance();
        };
        
        /**
         *Initialize the workflow instance.
         *@method init
         *@param {String} workflowData Id of an HTML container within which the worlflow is to be rendered
         *@param {Object} veda global "veda" instance
         *@param {veda.IndividualModel} net individual of rdfs:type "v-wf:Net"
         *return {Object} instance Returns an initialized instance of the workflow object
         */
        jsWorkflow.Instance.prototype.init = function(workflowData, veda, net, template, container) {

            var 	instance,
                    windows,
                    addNewState,
                    bindStateEvents,
                    workflow,
                    canvasSizePx=10000,
                    elementId,
                    process,
                    mode='view',
                    max_process_depth=0;
            
            if (net.hasValue('rdf:type')) {
            	if (net['rdf:type'][0].id == 'v-wf:Net') {
            		mode='edit';
            		elementId = net.id;
            	}
            	if (net['rdf:type'][0].id == 'v-wf:Process') {
            		mode='view';
            		process = net;
            		elementId = process.id;
            		net = (net.hasValue('v-wf:instanceOf'))?net['v-wf:instanceOf'][0]:[];            		
            	}
            }

            if (typeof workflowData === 'object') {
                workflow = workflowData.container;
                jsWorkflow.Instance.createWorkflowDOM(workflowData);
            } else {
                workflow = workflowData;
            }
            net['offsetX'] = localStorage.getItem("workflow"+elementId+"-offsetX");
            net['offsetY'] = localStorage.getItem("workflow"+elementId+"-offsetY");
            net['currentScale'] = localStorage.getItem("workflow"+elementId+"-zoom");
            if (net['currentScale']==null) net['currentScale'] = 1.0;
            
            if (!net['offsetX']) {
            	net['offsetX'] = 0;
            }
            if (!net['offsetY']) {
            	net['offsetY'] = 0;
            }
            
            $('#'+workflowData).css({
       			'height': canvasSizePx +'px',
       			'width': canvasSizePx+'px'
       		});
        	$('body').css('height','100vh');
        	$('#main').addClass('calculated-height');
        	$('#'+workflowData).draggable({
                drag: function (event, ui) {
                  instance.moveCanvas(ui.position.left, ui.position.top);	
              	  $("#workflow-context-menu").hide();
                }
            }).on("click", function() {
            	$("#workflow-context-menu").hide();
            });

            instance = this.instance;

            // Import all the given defaults into this instance.
            instance.importDefaults({
                Endpoint: "Dot",
                HoverPaintStyle: {
                    strokeStyle: "#6699FF",
                    lineWidth: 1
                },
                ConnectionOverlays: [
                    ["Arrow", {
                            location: 1,
                            id: "arrow",
                            length: 14,
                            width: 10,
                            foldback: 0.8
                        }],
                    ["Label", {
                            label: "transition",
                            id: "label",
                            cssClass: "aLabel"
                        }]
                ],
                Container: workflow // Id of the workflow container.
            });
            
            instance.moveCanvas = function (newLeft, newTop) {
            	//DEBUG $('#workflow-net-name').text(newLeft+" / "+newTop); 
            	
            	// change scale and offset
                $('#'+workflowData).css({
           			'left': (newLeft)+'px',
           			'top': (newTop)+'px',
           		});
                localStorage.setItem("workflow"+elementId+"-offsetX", newLeft);
                localStorage.setItem("workflow"+elementId+"-offsetY", newTop);
                net['offsetX'] = newLeft;
                net['offsetY'] = newTop;
            }
            if (net['offsetX']!=null && net['offsetX']!=0) {
            	instance.moveCanvas(net['offsetX'], net['offsetY']);
            } else {
                instance.moveCanvas(-canvasSizePx/2, -canvasSizePx/2);
            }

            // Bind a click listener to each transition (connection). On double click, the transition is deleted.
            if (mode=='edit') {
	            instance.bind("dblclick", function(transition) {
	                var _this = this;
	            	riot.route("#/individual/" + transition.id + "/#main//edit", true);
	/*
	                 if (mode=='edit' && confirm('Delete Flow?')) {
	                	 net['v-wf:consistsOf'] = veda.Util.removeSubIndividual(net, 'v-wf:consistsOf', transition.id);
	                	 var source = new veda.IndividualModel(transition.sourceId);
	            		 source['v-wf:hasFlow'] = veda.Util.removeSubIndividual(source, 'v-wf:hasFlow', transition.id);
	                	 instance.detach(transition, {fireEvent:false});
	                 }
	                 */
	            });
            }
            
            // Fill info panel on flow click
            instance.bind("click", function(transition) {
            	var _this = this, currentElement = $(_this), properties;
                properties = $('#workflow-selected-item');
                
                $('#'+veda.Util.escape4$(properties.find('#workflow-item-id').val())).removeClass('w_active');
                if (properties.find('#workflow-item-source').val()!=null) {
                	instance.select({source:properties.find('#workflow-item-source').val()}).each(function(e) {
                        e.setPaintStyle({strokeStyle: "#666666"});
                	});
                };
                
                transition.setPaintStyle({strokeStyle: "#FF0000"});

                if (transition.id == '__label') {
                	transition = transition.component;
                }
                
                properties.find('#workflow-item-id').val(transition.id);
                properties.find('#workflow-item-type').val('flow');
                properties.find('#workflow-item-source').val(transition.sourceId);
            });
            
            instance.bind("beforeDetach", function(connection) {
           		return connection.targetId.indexOf('jsPlumb') === 0;
            });
            
            // Handle creating new flow event
            instance.bind("connection", function(info) {
            	if (info.connection.id.indexOf('con')==-1) {
            		var flow = new veda.IndividualModel(info.connection.id);
          	    	flow["v-wf:flowsInto"] = [new veda.IndividualModel(info.targetId)]; // setup Flow target
            		return; // Don't use logic when we work with flows that already exists
            	}
                var individual = new veda.IndividualModel(); // create individual (Task / Condition) 
                individual.defineProperty("rdf:type");
                individual.defineProperty("rdfs:label");                
                individual.defineProperty("v-wf:flowsInto");
                
                individual["rdf:type"] = [veda.ontology["v-wf:Flow"]];
                individual["rdfs:label"] = [new String('')];
                
                net['v-wf:consistsOf'] = net['v-wf:consistsOf'].concat([individual]); // <- Add new Flow to Net
                
                var source = new veda.IndividualModel(info.sourceId);
                if (!source.hasOwnProperty('v-wf:hasFlow')) {
                	source.defineProperty('v-wf:hasFlow');
    			}
                source['v-wf:hasFlow'] = source['v-wf:hasFlow'].concat([individual]);
                
              	individual["v-wf:flowsInto"] = [new veda.IndividualModel(info.targetId)]; // setup Flow target
                
                info.connection.id = individual.id;
            });

            subNetViewButton = function(state, $state) {
            	if (!state.hasValue('v-wf:subNet')) {
            		return;
            	}
            	$("<span/>", {
            		"click": (function (instance) {
            	    	riot.route('#/individual/'+state['v-wf:subNet'][0].id+'/#main//edit', true);
            		 }),
            		 "class" : "glyphicon glyphicon-search subnet-link"
             	}).appendTo($state);
            };
            
            updateSVGBackground = function(item) {
                var svgBackground = "";
                if (item.hasClass('split-and')) {
                    svgBackground += "<line x1='80' y1='25' x2='100' y2='0' style='stroke:rgb(0,0,0); stroke-width:1' /><line x1='80' y1='0' x2='80' y2='50' style='stroke:rgb(0,0,0); stroke-width:1' /><line x1='80' y1='25' x2='100' y2='50' style='stroke:rgb(0,0,0); stroke-width:1' />";
                }
                if (item.hasClass('split-or')) {
                    svgBackground += "<line x1='100' y1='25' x2='90' y2='0' style='stroke:rgb(0,0,0); stroke-width:1' /><line x1='90' y1='0' x2='80' y2='25' style='stroke:rgb(0,0,0); stroke-width:1' /><line x1='80' y1='0' x2='80' y2='50' style='stroke:rgb(0,0,0); stroke-width:1' /><line x1='100' y1='25' x2='90' y2='50' style='stroke:rgb(0,0,0); stroke-width:1' /><line x1='90' y1='50' x2='80' y2='25' style='stroke:rgb(0,0,0); stroke-width:1' />";
                }
                if (item.hasClass('split-xor')) {
                    svgBackground += "<line x1='100' y1='25' x2='80' y2='0' style='stroke:rgb(0,0,0); stroke-width:1' /><line x1='80' y1='0' x2='80' y2='50' style='stroke:rgb(0,0,0); stroke-width:1' /><line x1='100' y1='25' x2='80' y2='50' style='stroke:rgb(0,0,0); stroke-width:1' />";
                }
                if (item.hasClass('join-and')) {
                    svgBackground += "<line x1='20' y1='25' x2='0' y2='0' style='stroke:rgb(0,0,0); stroke-width:1' /><line x1='20' y1='0' x2='20' y2='50' style='stroke:rgb(0,0,0); stroke-width:1' /><line x1='20' y1='25' x2='0' y2='50' style='stroke:rgb(0,0,0); stroke-width:1' />";
                }
                if (item.hasClass('join-or')) {
                    svgBackground += "<line x1='0' y1='25' x2='10' y2='0' style='stroke:rgb(0,0,0); stroke-width:1' /><line x1='10' y1='0' x2='20' y2='25' style='stroke:rgb(0,0,0); stroke-width:1' /><line x1='20' y1='0' x2='20' y2='50' style='stroke:rgb(0,0,0); stroke-width:1' /><line x1='0' y1='25' x2='10' y2='50' style='stroke:rgb(0,0,0); stroke-width:1' /><line x1='10' y1='50' x2='20' y2='25' style='stroke:rgb(0,0,0); stroke-width:1' />";
                }
                if (item.hasClass('join-xor')) {
                    svgBackground += "<line x1='0' y1='25' x2='20' y2='0' style='stroke:rgb(0,0,0); stroke-width:1' /><line x1='20' y1='0' x2='20' y2='50' style='stroke:rgb(0,0,0); stroke-width:1' /><line x1='0' y1='25' x2='20' y2='50' style='stroke:rgb(0,0,0); stroke-width:1' />";
                }
                svgBackground = "url(\"data:image/svg+xml;utf8,<svg xmlns='http://www.w3.org/2000/svg' version='1.1' preserveAspectRatio='none' viewBox='0 0 100 50'>" + svgBackground + "</svg>\")";
                item.css('background', svgBackground);
            };
            
            showProcessRunPath = function(workItem, depth) {
            	if (workItem.hasValue('v-wf:previousWorkItem')) {
            		workItem['v-wf:previousWorkItem'].forEach(function(previousWorkItem) {
            			if (workItem.hasValue('v-wf:forNetElement') && previousWorkItem.hasValue('v-wf:forNetElement')) {
            				showProcessRunPath(previousWorkItem, depth+1);
            				instance.select({target:workItem['v-wf:forNetElement'][0].id, source:previousWorkItem['v-wf:forNetElement'][0].id}).each(function(e) {
            					e.addClass('process-path-highlight');
            					e.setLabel(((e.getLabel()!='')?e.getLabel()+',':'')+(max_process_depth-depth));
            				});
            			}
            		});
            	} else {
            		max_process_depth = depth;
            	}
            };
            
            /**
             *Bind required functional to State elements
             *@method bindStateEvents
             *@param {Object} windows List of all State elements
             */
            bindStateEvents = function(windows) {
				var props = $("#props", template);
                
                windows.bind("click", function(e) {
                	instance.repaintEverything();
                	
                    var _this = this, currentElement = $(_this), properties, itemId;
                    properties = $('#workflow-selected-item');

                    if (properties.find('#workflow-item-source').val()!=null) {
                    	instance.select({source:properties.find('#workflow-item-source').val()}).each(function(e) {
                            e.setPaintStyle({strokeStyle: "#666666"});
                    	});
                    };
                    $('#'+veda.Util.escape4$(properties.find('#workflow-item-id').val())).removeClass('w_active'); // deactivate old selection
                    properties.find('#workflow-item-id').val(_this.id);
                    properties.find('#workflow-item-type').val('state');
                    currentElement.addClass('w_active');
                    
                	// build run path
                    if (mode=='view') {
                		instance.select().removeClass('process-path-highlight').setLabel('');
                		// If we have more then one WorkItem - we must choose among them 
                    	if (currentElement.attr('work-items-count')>1) {
                    		e.type = 'contextmenu';
                    		currentElement.trigger(e);
                    	} else { 
							props.empty();
                        	var s = new veda.IndividualModel();
    	                	s["rdf:type"]=[ veda.ontology["v-fs:Search"] ];
    	                	s.search("'rdf:type' == 'v-wf:WorkItem' && 'v-wf:forProcess' == '"+process.id+"' && 'v-wf:forNetElement'=='"+_this.id+"'");
    	                	for (var el in s.results) {
   	                	    	showProcessRunPath(s.results[el], 0);
   	                	    	var holder = $("<div>");
   	                	    	s.results[el].present(holder, new veda.IndividualModel("v-wf:WorkItemTemplate"));
   	                	    	props.append(holder);
    	                	}
                    	}
                    }
                });
                
                if (mode=='view') {
	                windows.bind("contextmenu", function(e, extra) {
	                	var _this = this,
	                	    menu = $("#workflow-context-menu ul");
	                	menu.html('');
	                	
	                	var s = new veda.IndividualModel();
	                	s["rdf:type"]=[ veda.ontology["v-fs:Search"] ];
	                	s.search("'rdf:type' == 'v-wf:WorkItem' && 'v-wf:forProcess' == '"+process.id+"' && 'v-wf:forNetElement'=='"+_this.id+"'");
	                	for (var el in s.results) {
						  var wi = s.results[el];
						  var $item = $("<li/>").appendTo(menu);
						  $("<a/>", {
							   "text" : (wi.hasValue('rdfs:label')?wi['rdfs:label'][0]:wi.id), 
							   "href" : '#',
							   "click" : (function (wi) {
									return function (event) {
										event.preventDefault();
										props.empty();
										$("#workflow-context-menu").hide();
										showProcessRunPath(wi, 0);
										var holder = $("<div>");
										wi.present(holder, new veda.IndividualModel("v-wf:WorkItemTemplate"));
										props.append(holder);
									};
								})(wi)
						  }).appendTo($item);
	                	}
	                	// 	                	
	                	$contextMenu.css({
	                	   display: "block",
	                	   left: e.pageX-((e.pageX+$contextMenu.width()>$( document ).width())?$contextMenu.width():0),
	                	   top: e.pageY-((e.pageY+$contextMenu.height()>$( document ).height())?$contextMenu.height():0)
	                	});
	                	return false;
	                });
                }
                if (mode=='edit') {
	                windows.bind("contextmenu", function(e) {
	                	var _this = this,
	                	    menu = $("#workflow-context-menu ul");
	                	menu.html('');
	                	// Add starting mappings to context menu
	                	var state = new veda.IndividualModel(_this.id);
	                	  if (state.hasValue('v-wf:startingMapping')) {
	                	     state['v-wf:startingMapping'].forEach(function(var_map) {
	                    	   var $item = $("<li/>").appendTo(menu);
	                	       var varId = null;
	                	       var_map['v-wf:mapToVariable'].forEach(function(var_var) {
	                	    	   varId = var_var.id;
	               	    		   $("<a/>", { 
	               	    			   "text" : (var_var.hasValue('v-wf:variableName')?var_var['v-wf:variableName'][0]:var_var.id), 
	               	    			   "href" : "#/individual/"+var_var.id+"/#main//edit"
	               	    		   }).appendTo($item);
	            	    	   });
	                	       $("<span/>", {"text": " <<< "}).appendTo($item);
	                	       var_map['v-wf:mappingExpression'].forEach(function(map_exp) {
	                	    	   $("<a/>", { 
	               	    			   "text" : map_exp, 
	               	    			   "href" : "#/individual/"+var_map.id+"/#main//edit"
	               	    		   }).appendTo($item);
	                	    	   $("<span/>", {
	                					"click": (function (instance) {
	                						return function (event) {
	                							event.preventDefault();
	                							instance.removeVarProperty(_this.id, varId, var_map.id);
	                							$(_this).trigger('contextmenu');
	                						};
	                					})(instance), 
	               	    			   "href" : ""
	               	    		   }).attr("class", "btn btn-default glyphicon glyphicon-remove button").attr("style", "padding: 3px;").appendTo($item);
	                	       });
	                        });
	                	  }
	                	  // Add completed mappings to context menu
	                	  if (state.hasValue('v-wf:completedMapping')) {
	                 	     state['v-wf:completedMapping'].forEach(function(var_map) {
	                     	   var $item = $("<li/>").appendTo(menu);
	                 	       var varId = null;
	                 	       var_map['v-wf:mappingExpression'].forEach(function(map_exp) {
	                 	    	   $("<a/>", { 
	                	    			   "text" : map_exp, 
	                	    			   "href" : "#/individual/"+var_map.id+"/#main//edit"
	                	    		   }).appendTo($item);
	                     	       $("<span/>", {"text": " >>> "}).appendTo($item);
	                     	       var_map['v-wf:mapToVariable'].forEach(function(var_var) {
	                     	    	   varId = var_var.id;
	                    	    		   $("<a/>", { 
	                    	    			   "text" : (var_var.hasValue('v-wf:variableName')?var_var['v-wf:variableName'][0]:var_var.id), 
	                    	    			   "href" : "#/individual/"+var_var.id+"/#main//edit"
	                    	    		   }).appendTo($item);
	                 	    	   });
	                 	    	   $("<span/>", {
	                 					"click": (function (instance) {
	                 						return function (event) {
	                 							event.preventDefault();
	                 							instance.removeVarProperty(_this.id, varId, var_map.id);
	                 							$(_this).trigger('contextmenu');
	                 						};
	                 					})(instance), 
	                	    			   "href" : ""
	                	    		   }).attr("class", "btn btn-default glyphicon glyphicon-remove button").attr("style", "padding: 3px;").appendTo($item);
	                 	       });
	                         });
	                 	  }
	                	  // Add executors to context menu
	                	  if (state.hasValue('v-wf:executor')) {
	                       state['v-wf:executor'].forEach(function(el2) {
	                    	   var variable = new veda.IndividualModel(el2.id);
	                    	   var $item = $("<li/>").appendTo(menu);
	                    	   $("<a/>", {
	                    		   "text" : 'EXECUTOR : '+(el2.hasValue('rdfs:label')?el2['rdfs:label'][0]:el2.id), 
	           	    			   "href" : '#/individual/'+el2.id+'/#main//edit'
	                    	   }).appendTo($item);
	            	    	   $("<span/>", {
	           						"click": (function (instance) {
	           							return function (event) {
	           								event.preventDefault();
	           								instance.removeExecutorProperty(_this.id, el2.id);
	           								$(_this).trigger('contextmenu');
	           							};
	           						})(instance), 
	          	    			   "href" : ""
	          	    		   }).attr("class", "btn btn-default glyphicon glyphicon-remove button").attr("style", "padding: 3px;").appendTo($item);
	                       });
	                	  }
	                    
	                	// Button for add new input variable to task
	             	    var $item = $("<li/>").appendTo(menu);
	     	    	    $("<span/>", {
	     	    	    	"text" : "IN VAR",
	    					"click": (function (instance) {
	    						return function (event) {
	    							event.preventDefault();
	    							instance.addVarProperty(_this.id, 'input');
	    							$(_this).trigger('contextmenu');
	    						};
	    					})(instance), 
	   	    			   "href" : ""
	   	    		    }).attr("class", "btn btn-default glyphicon glyphicon-plus").appendTo($item);
	     	    	    
	                	// Button for add new output variable to task
	     	    	    $("<span/>", {
	     	    	    	"text" : "OUT VAR",
	    					"click": (function (instance) {
	    						return function (event) {
	    							event.preventDefault();
	    							instance.addVarProperty(_this.id, 'output');
	    							$(_this).trigger('contextmenu');
	    						};
	    					})(instance), 
	   	    			   "href" : ""
	   	    		    }).attr("class", "btn btn-default glyphicon glyphicon-plus").appendTo($item);
	     	    	    
	                	// Button for add new executor to task
	    	    	    $("<span/>", {
	    	    	    	"text" : "EXECUTOR",
	    	    	    	"click": (function (instance) {
	    	    	    		return function (event) {
	    	    	    			event.preventDefault();
	    	    	    			instance.addExecutorProperty(_this.id);
	    	    	    			$(_this).trigger('contextmenu');
	    	    	    		};
	    	    	    	})(instance), 
	  	    			   "href" : ""
	  	    		    }).attr("class", "btn btn-default glyphicon glyphicon-plus").appendTo($item);
	                	
	                	// 
	                	$contextMenu.css({
	                	   display: "block",
	                	   left: e.pageX-((e.pageX+$contextMenu.width()>$( document ).width())?$contextMenu.width():0),
	                	   top: e.pageY-((e.pageY+$contextMenu.height()>$( document ).height())?$contextMenu.height():0)
	                	});
	                	return false;
	                });
	
	                windows.bind("dblclick", function() {
	                    var _this = this;
	                	riot.route("#/individual/" + $(_this).attr('id')+"/#main//edit", true);
	                });
	
	                // Initialize State elements as draggable.  
	                instance.draggable(windows, {
	                  drag: function (event, ui) { //gets called on every drag
	                	  $("#workflow-context-menu").hide();
	                	  var target = new veda.IndividualModel(event.target.id);
	                   	  target['v-wf:locationX'] = [new Number(Math.round(ui.position.left-canvasSizePx/2))];
	                   	  target['v-wf:locationY'] = [new Number(Math.round(ui.position.top-canvasSizePx/2))];
	                  }
	            	});
                }

                // Initialize all State elements as Connection sources.
                var possibleInAnchors = [[0, 0.1,-1, 0],
                                        [0, 0.3,-1, 0],
                                        [0, 0.5,-1, 0],
                                        [0, 0.7,-1, 0],
                                        [0, 0.9,-1, 0],
                                        [1, 0.1, 1, 0],
                                        [1, 0.3, 1, 0],
                                        [1, 0.5, 1, 0],
                                        [1, 0.7, 1, 0],
                                        [1, 0.9, 1, 0],
                                        [0.1, 0, 0,-1],
                                        [0.3, 0, 0,-1],
                                        [0.5, 0, 0,-1],
                                        [0.7, 0, 0,-1],
                                        [0.9, 0, 0,-1]];
                var possibleOutAnchors = [[0, 0.2,-1, 0],
                                       [0, 0.4,-1, 0],
                                       [0, 0.6,-1, 0],
                                       [0, 0.8,-1, 0],
                                       [1, 0.2, 1, 0],
                                       [1, 0.4, 1, 0],
                                       [1, 0.6, 1, 0],
                                       [1, 0.8, 1, 0],
                                       [0.2, 0, 0,-1],
                                       [0.4, 0, 0,-1],
                                       [0.6, 0, 0,-1],
                                       [0.8, 0, 0,-1]];
                instance.makeSource(windows, {
                    filter: ".ep",
                    anchor: possibleOutAnchors,
                    connector: [
						"Straight", {
                    	stub: 30,
                        gap: 0
						}
                    ],paintStyle:{ 
                        strokeStyle:"#225588",
                        fillStyle:"transparent",
                        radius:mode=='edit'?4:1,
                        lineWidth:1 
                    },
                    connectorStyle: {
                        strokeStyle: "#666666",
                        lineWidth: 1,
                        outlineColor: "transparent",
                        outlineWidth: 4
                    },
                    maxConnections: 20,
                    onMaxConnections: function(info, e) {
                        alert("Maximum connections (" + info.maxConnections + ") reached");
                    }
                });

                // Initialize all State elements as connection targets.
                
                instance.makeTarget(windows, {
                    dropOptions: {
                        hoverClass: "dragHover"
                    },
                    anchor: possibleInAnchors,
                    paintStyle:{ 
                        strokeStyle:"#225588",
                        fillStyle:"transparent",
                        radius:mode=='edit'?4:1,
                        lineWidth:1 
                    }
                });
            };

            /**
             * @method
             * Change current scale.
             * @param scale new scale
             */
            instance.changeScale = function(scale) {
            	$("#workflow-context-menu").hide();
            	
            	net['currentScale'] = parseFloat(scale);
            	localStorage.setItem("workflow"+elementId+"-zoom", net['currentScale']);
            	
            	instance.setZoom(net['currentScale']);
            	$('#'+workflowData).css({
            		'-ms-transform': 'scale('+net['currentScale']+','+net['currentScale']+')', /* IE 9 */
            		'-webkit-transform': 'scale('+net['currentScale']+','+net['currentScale']+')', /* Chrome, Safari, Opera */
            		'transform': 'scale('+net['currentScale']+','+net['currentScale']+')'
            	});
            };
            
            /**
             * @method getSplitJoinType
             * Generate css class for state (split-[xor-or-and-none] or join-[xor-or-and-none])
             * @param {String} sj `split` or `join`
             * @param {veda.IndividualModel} state state
             * @return css class name for this type of split/join  
             */
            instance.getSplitJoinType = function(sj, state) {
            	if (!state.hasValue('v-wf:'+sj)) {
            		return ' '+sj+'-no';
            	}
            	var type = state['v-wf:'+sj][0].id;
            	if (type === null || type === undefined || type === '') {
            		return ' '+sj+'-no';
            	}
            	
            	if (type == 'v-wf:XOR')  return ' '+sj+'-xor';
            	if (type == 'v-wf:OR')   return ' '+sj+'-or';
            	if (type == 'v-wf:AND')  return ' '+sj+'-and';
            	if (type == 'v-wf:NONE') return ' '+sj+'-none';
            	
            	return ' '+sj+'-no';
            };
            
            /**
             * @method
             * Apply state to canvas
             */
            instance.createState = function(state) {
            	if (!state.hasValue('rdf:type')) return;
            	var type = state['rdf:type'][0].id;
            	var stateElement = '';
            	switch (type) {
    			case 'v-wf:InputCondition':    				
    				stateElement = '<div class="w state-condition" ' + 
    				    'id="' + state.id + '" ' + 
    				    'style="font-size:20px;padding-top:10px;'+ 
    				    'left:' + (canvasSizePx/2+state['v-wf:locationX'][0]) + 'px;' +
    				    'top:' + (canvasSizePx/2+state['v-wf:locationY'][0]) + 'px;">' +
					    '<div><span class="glyphicon glyphicon-play" aria-hidden="true"></div>' +
					    (mode=='edit'?'<div class="ep">':'')+'</div></div>';
    				break;
    			case 'v-wf:OutputCondition':
    				stateElement = '<div class="w state-condition" ' +
    				    'id="' + state.id + '" ' +
    				    'style="font-size:20px;padding-top:10px;' +
    				    'left:' + (canvasSizePx/2+state['v-wf:locationX'][0]) + 'px;' + 
    				    'top: ' + (canvasSizePx/2+state['v-wf:locationY'][0]) + 'px;">' +
					    '<div><span class="glyphicon glyphicon-stop" aria-hidden="true"></div></div>';
    				break;
    			case 'v-wf:Condition':
    				stateElement = '<div class="w state-condition" ' +
    				    'id="' + state.id + '" ' + 
    				    'style="left:' + (canvasSizePx/2+state['v-wf:locationX'][0]) + 'px;' + 
    				    'top:' + (canvasSizePx/2+state['v-wf:locationY'][0]) + 'px;">' +
    				    '<div class="state-name"></div>' + 
    				    (mode=='edit'?'<div class="ep">':'')+'</div></div>';
    				break;
    			case 'v-wf:Task':    				
            		stateElement = '<div class="w state-task split-join ' +
					    instance.getSplitJoinType('split', state) +
					    instance.getSplitJoinType('join', state) + '" '+
					    'id="' + state.id + '" ' +
					    'style="left:' + (canvasSizePx/2+state['v-wf:locationX'][0]) + 'px; ' + 
					    'top: ' + (canvasSizePx/2+state['v-wf:locationY'][0]) + 'px;">' + 
					    '<div class="state-name">' + state['rdfs:label'][0] + '</div>' +
					    (mode=='edit'?'<div class="ep">':'')+'</div></div>';
    				break;
    			}            	
            	if (stateElement!=='') {
                	$('#'+workflowData).append(stateElement);
                	var $state = $('#' + veda.Util.escape4$(state.id));
                	bindStateEvents($state);
                	if (mode=='edit') subNetViewButton(state, $state);
                	updateSVGBackground($state);
            	}
            };
            
            instance.addExecutorProperty = function(stateId) {
            	
                executorName = prompt("Enter name of the executor");
                if (executorName==null) return;
                
                var individualE = new veda.IndividualModel(); // create individual (Executor) 
                individualE.defineProperty("rdf:type");
                individualE.defineProperty("rdfs:label");
                individualE.defineProperty("v-s:script");
                
           		individualE["rdf:type"] = [veda.ontology["v-wf:ExecutorDefinition"]];
                individualE['rdfs:label'] = ['Executor `'+executorName+'`'];
                
                var state = veda.IndividualModel(stateId);
               	state['v-wf:executor'] = (state['v-wf:executor'] === undefined)?[individualE]:state['v-wf:executor'].concat([individualE]); // <- Add new Executor to State
            };
            
            instance.addVarProperty = function(stateId, type) {            
                variableName = prompt("Enter name of the variable");
                if (variableName==null) return;
                
                var individualV = new veda.IndividualModel(); // create individual (Variable) 
                individualV.defineProperty("rdf:type");
                individualV.defineProperty("rdfs:label");
                individualV.defineProperty("v-wf:variableName");
                
           		individualV["rdf:type"] = [veda.ontology["v-wf:Variable"]];
                individualV['rdfs:label'] = ['Variable `'+variableName+'`'];
                individualV['v-wf:variableName'] = [variableName];
                
                var individualM = new veda.IndividualModel(); // create individual (Mapping)
                
                individualM.defineProperty("rdf:type");
                individualM.defineProperty("v-wf:mapToVariable");
                individualM.defineProperty("v-wf:mappingExpression");
                
           		individualM["rdf:type"] = [veda.ontology["v-wf:Mapping"]];
           		individualM["v-wf:mapToVariable"] = [individualV];
                individualM['v-wf:mappingExpression'] = ["context.getVariableValue ('"+variableName+"')"];
                
                if (type=='input') {
                	var state = veda.IndividualModel(stateId);
               		state['v-wf:inputVariable'] = (state['v-wf:inputVariable'] === undefined)?[individualV]:state['v-wf:inputVariable'].concat([individualV]); // <- Add new Varibale to State
               		state['v-wf:startingMapping'] = (state['v-wf:startingMapping'] === undefined)?[individualM]:state['v-wf:startingMapping'].concat([individualM]); // <- Add new Mapping to State
                }
                if (type=='output') {
                	var state = veda.IndividualModel(stateId);
               		state['v-wf:outputVariable'] = (state['v-wf:outputVariable'] === undefined)?[individualV]:state['v-wf:outputVariable'].concat([individualV]); // <- Add new Varibale to State
               		state['v-wf:completedMapping'] = (state['v-wf:completedMapping'] === undefined)?[individualM]:state['v-wf:completedMapping'].concat([individualM]); // <- Add new Mapping to State
                }
            };
            
            // Remove from state, defined by stateId, variable `varId` and its mapping `mapId`
            instance.removeVarProperty = function(stateId, varId, mapId) {
            	var state = veda.IndividualModel(stateId);
           		state['v-wf:inputVariable'] = veda.Util.removeSubIndividual(state, 'v-wf:inputVariable', varId);
           		state['v-wf:startingMapping'] = veda.Util.removeSubIndividual(state, 'v-wf:startingMapping', mapId);
            };
            
            instance.removeExecutorProperty = function(stateId, executorId) {
            	var state = veda.IndividualModel(stateId);   
           		state['v-wf:executor'] = veda.Util.removeSubIndividual(state, 'v-wf:executor', executorId);
            };
            
            instance.deleteState = function(element) {
            	instance.detachAllConnections(element);
            	instance.remove(element);
            	net['v-wf:consistsOf'] = veda.Util.removeSubIndividual(net, 'v-wf:consistsOf', element.id);
            };
            
            instance.createFlow = function(state, flow) {
            	var connector = instance.connect({
            		id: flow.id,
                    source: state.id,
                    target: flow['v-wf:flowsInto'][0].id,
                    detachable:(mode=='edit')
                });
            };
            
            instance.deleteFlow = function(flow, source) {
	          	net['v-wf:consistsOf'] = veda.Util.removeSubIndividual(net, 'v-wf:consistsOf', flow.id);
	           	var source = new veda.IndividualModel(source.id);
	       		source['v-wf:hasFlow'] = veda.Util.removeSubIndividual(source, 'v-wf:hasFlow', flow.id);
	           	instance.detach(flow, {fireEvent:false});
            }
            
            /**
             *Create workflow Net by given Object (v-wf:Net individual).
             *@method createNetView A public method
             *@param {Object} workflowData A workflow object to create State transitions
             */
            instance.createNetView = function(net) {
            	$('#workflow-net-name').text(net['rdfs:label'][0]);
            	// Create States
            	net['v-wf:consistsOf'].forEach(function(el) {
            		el['rdf:type'].forEach(function (type) {
            			instance.createState(el);
            		});
            	});
            	
            	// Create Flows
            	net['v-wf:consistsOf'].forEach(function(el) {
            		if (undefined !== el['v-wf:hasFlow']) {
            			el['v-wf:hasFlow'].forEach(function (flow) {
            				instance.createFlow(el, flow);
            			});
            		}
            	});
            };
            
            /*
             * Optimize view of net: all elements must be visible and fit screen (through change scale and position of canvas)
             * @returns 
             */
            instance.optimizeView = function() {
            	if (!net.hasValue('v-wf:consistsOf')) return;
            	var minx, maxx, miny, maxy, scale, 
                  offsetX = 0, offsetY = 0;
            	// read ranges
            	net['v-wf:consistsOf'].forEach(function(state) {
            		if (state.hasValue('v-wf:locationX')) {
            			if (maxx === undefined || state['v-wf:locationX'][0]>maxx) maxx = state['v-wf:locationX'][0]; 
            			if (minx === undefined || state['v-wf:locationX'][0]<minx) minx = state['v-wf:locationX'][0];
            		}
            		if (state.hasValue('v-wf:locationY')) {
            			if (maxy === undefined || state['v-wf:locationY'][0]>maxy) maxy = state['v-wf:locationY'][0]; 
            			if (miny === undefined || state['v-wf:locationY'][0]<miny) miny = state['v-wf:locationY'][0];
            		}
            	});

            	// TODO update this from css;
            	miny-=25;
            	minx-=25;
            	maxx+=100;
            	maxy+=100;
            	
                
            	// read viewport div
            	$(".workflow-canvas-wrapper").each(function() {
            		var scaleX = this.clientWidth/(maxx-minx);
            		var scaleY = this.clientHeight/(maxy-miny);
            		scale = Math.min(scaleX, scaleY);
            		if (scaleX>scaleY) {
            			offsetX = (this.clientWidth - (maxx-minx)*scale) /2;
            		} else {
            			offsetY = (this.clientHeight - (maxy-miny)*scale) /2;
            		}
            	});
                instance.changeScale(scale);
                instance.moveCanvas(-minx*scale+offsetX-canvasSizePx/2, -miny*scale+offsetY-canvasSizePx/2);
            };
            
            instance.addProcessVariable = function(individualProperty, listId) {
            	if (process.hasValue(individualProperty)) {
                	$iv = $(listId);
                	process[individualProperty].forEach(function(el) {
                  	   var $item = $("<li/>").appendTo($iv);
                	   $("<a/>", {
                		   "text" : (el.hasValue('v-wf:variableName')?el['v-wf:variableName'][0]:el.id),
       	    			   "href" : '#/individual/'+el.id+'/#main'
                	   }).appendTo($item);
                	});
            	}
            };
            
            instance.createProcessView = function(process, reload) {
            	// Apply WorkItems to Net
            	var s = new veda.IndividualModel();
            	s["rdf:type"]=[ veda.ontology["v-fs:Search"] ];
            	if (reload) {            		
            		s.search("'rdf:type' == 'v-wf:WorkItem' && 'v-wf:forProcess' == '"+process.id+"'", undefined, true);
            		$('.w').each(function(index) {
            			$("span", this ).text('');
            			$( this ).css('background-color', 'white').attr('work-items-count',0).attr('colored-to','');
            		});
            	} else {
            		s.search("'rdf:type' == 'v-wf:WorkItem' && 'v-wf:forProcess' == '"+process.id+"'");
            	}
            	for (var el in s.results) {
            	    if (s.results.hasOwnProperty(el)) {
            	    	var wi = s.results[el];
                		if (wi.hasValue('v-wf:forNetElement')) {
                			var state = $('#'+veda.Util.escape4$(wi['v-wf:forNetElement'][0].id));
                			var wic = parseInt(state.attr('work-items-count'));
					var red = state.attr('colored-to')=='red';    
                			if (wic>0) {                				
                				state.attr('work-items-count', wic+1);
						$(".counter", state).remove();
                				$("<span/>", {
					   			   "class" : "counter",    
                             		   "text" : 'x'+(wic+1)
                             	   }).appendTo(state);                				
                			} else {
                				state.attr('work-items-count', 1);
                			}
            				if (!wi.hasValue('v-wf:workOrderList')) {
                    			state.css('background-color', '#FF3333');
                    			state.attr('colored-to', 'red');
            				} else if (wi.hasValue('v-wf:isCompleted') && wi['v-wf:isCompleted'][0]==true && !red) {
                    			state.css('background-color', '#88B288');
                    			state.attr('colored-to', 'green');
            				} else if (!red) {
                    			state.css('background-color', '#FFB266');
                    			state.attr('colored-to', 'red');
            				}
                		}
            	    }
            	}
            	// Add variables to lists
            	instance.addProcessVariable('v-wf:inputVariable','#process-input-variables');
            	instance.addProcessVariable('v-wf:localVariable','#process-local-variables');
            	instance.addProcessVariable('v-wf:outputVariable','#process-output-variables');
            };

            instance.createNetView(net);
            if (mode=='view') {
            	instance.createProcessView(process);
            }
            
            if (net['currentScale']==1.0) {
            	instance.optimizeView();
            } else {
            	instance.changeScale(net['currentScale']);
            }            	
            
            /* CONTEXT MENU [BEGIN] */
            var $contextMenu = $("#workflow-context-menu");
            /* CONTEXT MENU [END]*/
            
            /* NET MENU [BEGIN] */
            $('#workflow-save-button').on('click', function() {
            	// TODO REFACTOR - recursive save (based on type checking)
           	  net.save();
        	  if (net.hasValue('v-wf:consistsOf')) {
        		  net['v-wf:consistsOf'].forEach(function(el) {
            		if (el.hasValue('v-wf:inputVariable')) {
                		el['v-wf:inputVariable'].forEach(function(v) {
                			v.save();
                		});
            		}
            		if (el.hasValue('v-wf:startingMapping')) {
            			el['v-wf:startingMapping'].forEach(function(m) {
            				m.save();
            			});
            		}
            		if (el.hasValue('v-wf:executor')) {
            			el['v-wf:executor'].forEach(function(e) {
            				e.save();
            			});
            		}
            		el.save();
        		 });
        	  }
            });
            
            $('#workflow-export-ttl').on('click', function() {
           		var list = new veda.IndividualListModel(net, net['v-wf:consistsOf']);
           		veda.Util.exportTTL(list);
            });

            // Add new State event.
            $(".create-state").bind("click", function() {
                var _this = this,
                        stateId,
                        stateElement, 
                        stateName = prompt("Enter name of the state");
                if (stateName==null) return;
                var individual = new veda.IndividualModel(); // create individual (Task / Condition) 
                                
                individual.defineProperty("rdf:type");
                individual.defineProperty("rdfs:label");
                individual.defineProperty("v-wf:locationX");
                individual.defineProperty("v-wf:locationY");

                individual['rdfs:label'] = [stateName, ''];
                individual['v-wf:locationX'] = [new Number((-canvasSizePx/2-net['offsetX'])/net['currentScale'])];
                individual['v-wf:locationY'] = [new Number((-canvasSizePx/2-net['offsetY'])/net['currentScale'])];
                
                if ($('#'+workflowData).find('#' + individual.id).length < 1) {

                   	if ($(_this).hasClass('create-condition')) {
                   		individual["rdf:type"] = [veda.ontology["v-wf:Condition"]];
                    	instance.createState(individual);
                    } else { 
                        individual["rdf:type"] = [veda.ontology["v-wf:Task"]];
                    	instance.createState(individual);
                    }

                   	net['v-wf:consistsOf'] = net['v-wf:consistsOf'].concat([individual]); // <- Add new State to Net	
                    $('#' + individual.id).click();
                } else {
                    alert('This state is already present.');
                }
                $(this).blur();
            });

            $('.delete-state').on('click', function() {
                if ($('#workflow-item-type').val()=='state') {
	                if (confirm('Delete state ' + $('#workflow-item-id').val() + ' ?')) {
	                	instance.deleteState(instance.getSelector('#'+veda.Util.escape4$($('#workflow-item-id').val()))[0]);
	                }
                }
                if ($('#workflow-item-type').val()=='flow') {
	                if (confirm('Delete flow ' + $('#workflow-item-id').val() + ' ?')) {
	                	instance.deleteFlow(instance.getSelector('#'+veda.Util.escape4$($('#workflow-item-id').val()))[0]);
	                }
                }
            });
            
            $('.process-refresh').on('click', function() {
            	instance.createProcessView(process, true);
            });
            
            /* ZOOM [BEGIN] */
            $('.zoom-in').on('click', function() {
            	if (net['currentScale']<1) return instance.changeScale(net['currentScale'] + 0.1);
            	if (net['currentScale']<2) return instance.changeScale(net['currentScale'] + 0.25);
            });

            $('.zoom-out').on('click', function() {
            	if (net['currentScale']>1) return instance.changeScale(net['currentScale'] - 0.25);
            	if (net['currentScale']>0.2) return instance.changeScale(net['currentScale'] - 0.1);
            });
            
            $('#'+workflowData).bind('mousewheel', function(e){
            	if(e.originalEvent.wheelDelta > 0) {
            		if (net['currentScale']<1) { return instance.changeScale(net['currentScale'] + 0.1); }
            			else if (net['currentScale']<2) return instance.changeScale(net['currentScale'] + 0.25);
                } else {
                    if (net['currentScale']>1) { return instance.changeScale(net['currentScale'] - 0.25) }
                    	else if (net['currentScale']>0.2) return instance.changeScale(net['currentScale'] - 0.1);
                }
            });
            
            $('.zoom-default').on('click', function() {            	
            	instance.optimizeView();
            });            
            /* ZOOM [END] */

            /* NET MENU [END] */
            
            return instance;
        };
    });
})();

//[END] Block of net editor
