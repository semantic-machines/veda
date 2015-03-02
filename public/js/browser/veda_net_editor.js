/**
 * NET EDITOR
 * 
 * Inspired by http://github.com/hemantsshetty/jsWorkflow
 */


function getSubIndividual(net, property, id, func) {
	net[property].forEach(function(el) {
		if (el.id == id) {
			func(el);
		}
	});
}

function removeSubIndividual(net, property, id) {
	return net[property].filter( function (item) {
		return item.id !== id; 
	});
}

//[BEGIN] Block of net editor

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
        }
        /**
         *Initialize the workflow instance.
         *@method init
         *@param {String} workflowData Id of an HTML container within which the worlflow is to be rendered
         *@param {Object} workflowData A workflow object to render new workflow State elements in the DOM
         *return {Object} instance Returns an initialized instance of the workflow object
         */
        jsWorkflow.Instance.prototype.init = function(workflowData, veda, net) {

            var 	instance,
                    windows,
                    addNewState,
                    bindStateEvents,
                    workflow,
                    canvasSizePx=10000,
                    currentScale=1;

            if (typeof workflowData === 'object') {
                workflow = workflowData.container;
                jsWorkflow.Instance.createWorkflowDOM(workflowData);
            } else {
                workflow = workflowData;
            }
            net['offsetX'] = localStorage.getItem("workflow"+net.id+"-offsetX");
            net['offsetY'] = localStorage.getItem("workflow"+net.id+"-offsetY");
            
            if (!net['offsetX']) {
            	net['offsetX'] = 0;
            }
            if (!net['offsetY']) {
            	net['offsetY'] = 0;
            }
            
            $('#'+workflowData).css({
       			'height': canvasSizePx +'px',
       			'width': canvasSizePx+'px',
       			'left': (-net['offsetX']-canvasSizePx/2)+'px',
       			'top': (-net['offsetY']-canvasSizePx/2)+'px',
       		});
        	$('body').css('height','100vh');
        	$('#main').addClass('calculated-height');
        	$('#'+workflowData).draggable({
                drag: function (event, ui) {
                  localStorage.setItem("workflow"+net.id+"-offsetX", -ui.position.left-canvasSizePx/2);
                  localStorage.setItem("workflow"+net.id+"-offsetY", -ui.position.top-canvasSizePx/2); 
              	  $("#workflow-context-menu").hide();
                }
            });

            instance = this.instance;

            // Import all the given defaults into this instance.
            instance.importDefaults({
                Endpoint: ["Dot", {
                        radius: 0.1
                    }],
                HoverPaintStyle: {
                    strokeStyle: "#6699FF",
                    lineWidth: 2
                },
                ConnectionOverlays: [
                    ["Arrow", {
                            location: 1,
                            id: "arrow",
                            length: 14,
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

            // Bind a click listener to each transition (connection). On double click, the transition is deleted.
            instance.bind("dblclick", function(transition) {            	 
                 if (confirm('Delete Flow?')) {
                	 net['v-wf:consistsOf'] = removeSubIndividual(net, 'v-wf:consistsOf', transition.id);
                	 getSubIndividual(net, 'v-wf:consistsOf', transition.sourceId, function (el) {
                		 el['v-wf:hasFlow'] = removeSubIndividual(el, 'v-wf:hasFlow', transition.id);
                	 });
                	 instance.detach(transition);
                 }
            });
            
            // Fill info panel on flow click
            instance.bind("click", function(transition) {
            	var _this = this, currentElement = $(_this), properties;
                properties = $('#workflow-selected-item');
                $('#'+properties.find('#workflow-item-id').val()).removeClass('w_active');
                
                if (transition.id == '__label') {
                	transition = transition.component;
                }
                
                properties.find('#workflow-item-id').val(transition.id);
                properties.find('#workflow-item-type').val('flow');
                // properties.find('#workflow-item-label').val(transition.getLabel());
                currentElement.addClass('w_active');                
               	// $('.task-buttons').hide();
            });
            
            // Handle creating new flow event
            instance.bind("connection", function(info) {
            	if (info.connection.id.indexOf('con')==-1) {
            		return; // Don't use logic when we work with flows that already exists
            	}
                var individual = new veda.IndividualModel(); // create individual (Task / Condition) 
                individual.defineProperty("rdf:type");
                individual.defineProperty("rdfs:label");                
                individual.defineProperty("v-wf:flowsInto");
                
                individual["rdf:type"] = [veda.ontology["v-wf:Flow"]];
                individual["rdfs:label"] = [new String('')];
                
                net['v-wf:consistsOf'] = net['v-wf:consistsOf'].concat([individual]); // <- Add new Flow to Net
                
                getSubIndividual(net, 'v-wf:consistsOf', info.sourceId, function(el) {
                	if (!('v-wf:hasFlow' in el)) {
        				el.defineProperty('v-wf:hasFlow');
        			}
        			el['v-wf:hasFlow'] = el['v-wf:hasFlow'].concat([individual]); // <- Add new Flow to State
                });
                
                getSubIndividual(net, 'v-wf:consistsOf', info.targetId, function(el) {
                	 individual["v-wf:flowsInto"] = [el]; // setup Flow source
                });
                
                info.connection.id = individual.id;
            });

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
            }
            
            /**
             *Bind required functionalities to State elements
             *@method bindStateEvents
             *@param {Object} windows List of all State elements
             */
            bindStateEvents = function(windows) {

                // По клику переходим на свойства объекта
                windows.bind("click", function() {

                	instance.repaintEverything();
                	
                    var _this = this, currentElement = $(_this), properties, itemId;
                    properties = $('#workflow-selected-item');
                                        
                    $('#'+veda.Util.escape4$(properties.find('#workflow-item-id').val())).removeClass('w_active'); // deactivate old selection
                    properties.find('#workflow-item-id').val(_this.id);
                    properties.find('#workflow-item-type').val('state');
                    /*
                    properties.find('#workflow-item-label').val(currentElement.find('.state-name').text());                    

                    ["no", "and", "or", "xor"].forEach(function(entry) {
                        if (currentElement.hasClass('split-'+entry)) {
                        	$('#workflow-split-buttons label').removeClass('active').find('input').attr("checked",false);                        	
                        	$('input[name=item-split-type][value='+entry+']').attr("checked",true).parent().addClass('active');
                        }                        
                        if (currentElement.hasClass('join-'+entry)) {
                        	$('#workflow-join-buttons label').removeClass('active').find('input').attr("checked",false);                        	
                        	$('input[name=item-join-type][value='+entry+']').attr("checked",true).parent().addClass('active');
                        }
                    });
                    */          
                    /*
                    if (currentElement.hasClass('state-condition')) {
                    	$('.task-buttons').hide();
                    } else {
                    	$('.task-buttons').show();
                    }*/
                    currentElement.addClass('w_active');
                });

                // Bind a click listener to each State elements. On double click, State elements are deleted.
                windows.bind("dblclick", function() {
                    var _this = this;
                	riot.route("#/document/" + $(_this).attr('id')+"///edit", true);
                });

                // Initialize State elements as draggable.  
                instance.draggable(windows, {
                  drag: function (event, ui) { //gets called on every drag
                	  $("#workflow-context-menu").hide();
                      getSubIndividual(net, 'v-wf:consistsOf', event.target.id, function(el) {
              			  el['v-wf:locationX'] = [new Number(Math.round(ui.position.left-canvasSizePx/2))];
            			  el['v-wf:locationY'] = [new Number(Math.round(ui.position.top-canvasSizePx/2))];
                      });
                  }
            	});

                // Initialize all State elements as Connection sources.
                instance.makeSource(windows, {
                    filter: ".ep",
                    anchor: ["Continuous", { faces:[ "top", "left", "right" ] } ],
                    connector: [
						/*"Bezier", {
                    	curviness: 50
						}*/
						"Straight", {
                    	stub: 30,
                        gap: 0
						}
						/*"Flowchart", {
                    	alwaysRespectStubs: false,
                        stub: 0,
                        gap: 0,
                        midpoint: 0.5,
                        cornerRadius: 0
						}*/
						/*"StateMachine", {
                    	margin: 5,
                    	curviness: 20,
                        proximityLimit: 100
						}*/
                    ],
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
                    }
                	,anchor: ["Continuous", { faces:[ "top", "left", "right" ] } ]
                });
            };

            // Add new State event.
            jsPlumb.getSelector(".create-state").bind("click", function() {
                var _this = this,
                        stateName,
                        stateId,
                        stateElement, 
                        individual = new veda.IndividualModel(); // create individual (Task / Condition) 
                                
                individual.defineProperty("rdf:type");
                individual.defineProperty("rdfs:label");
                individual.defineProperty("v-wf:locationX");
                individual.defineProperty("v-wf:locationY");

                stateName = prompt("Enter the name of the state");
                
                individual['rdfs:label'] = [new String(stateName.replace(/[^a-zA-Z0-9 ]/g, ''))];
                individual['v-wf:locationX'] = [new Number(1)];
                individual['v-wf:locationY'] = [new Number(1)];
                
                if ($('#'+workflowData).find('#' + individual.id).length < 1) {

                   	if ($(_this).hasClass('create-condition')) {
                   		individual["rdf:type"] = [veda.ontology["v-wf:Condition"]];
                    	instance.createState('v-wf:Condition', individual);
                    } else { 
                        individual["rdf:type"] = [veda.ontology["v-wf:Task"]];
                    	instance.createState('v-wf:Task', individual);
                    }

                   	net['v-wf:consistsOf'] = net['v-wf:consistsOf'].concat([individual]); // <- Add new State to Net	
                    $('#' + individual.id).click();
                } else {
                    alert('This state is already present.');
                }
                $(this).blur();
            });

            /**
             *Get all State transitions
             *@method getStateTransitions A public method
             *@return {Object} workflowTransition List of all States and their transition (connection) with other States
             */
            instance.getStateTransitions = function() {
                var plumbConnections = instance.getAllConnections(),
                        connectionCount = plumbConnections.length,
                        workflowTransition = {},
                        sourceID,
                        targetID;

                for (var i = 0; i < connectionCount; i += 1) {

                    sourceID = plumbConnections[i]['sourceId'];
                    targetID = plumbConnections[i]['targetId'];

                    workflowTransition[sourceID] = (workflowTransition[sourceID]) ?
                            (workflowTransition[sourceID] + ',' + plumbConnections[i]['targetId']) :
                            plumbConnections[i]['targetId'];
                }
                return workflowTransition;
            }

            /**
             *Get all State names
             *@method getStateNames A public method
             *@return {Object} workflowStateName List of all State Element Ids with their respective names
             */
            instance.getStateNames = function() {
                var stateCount = windows.length,
                        workflowStateName = {};

                for (var i = 0; i < stateCount; i += 1) {

                    workflowStateName[$(windows[i]).attr('id')] = $(windows[i]).text().trim();
                }
                return workflowStateName;
            }

            /**
             *Get all State position
             *@method getStatePositions A public method
             *@return {Object} workflowStatePosition List of all State Element Ids with their respective css positions (top and left)
             */
            instance.getStatePositions = function() {

                // Get updates array of State elements.
                windows = jsPlumb.getSelector("." + workflow + " .w");                
                var stateCount = windows.length,
                        workflowStatePosition = {};

                for (var i = 0; i < stateCount; i += 1) {

                    workflowStatePosition[$(windows[i]).attr('id')] = $(windows[i]).position();
                }
                return workflowStatePosition;
            }
            
            instance.changeScale = function(scale) {
            	$("#workflow-context-menu").hide();
            	currentScale = scale;
            	instance.setZoom(currentScale);
            	localStorage.setItem("workflow"+net.id+"-zoom", currentScale);
            	$('#'+workflowData).css({
            		'-ms-transform': 'scale('+currentScale+','+currentScale+')', /* IE 9 */
            		'-webkit-transform': 'scale('+currentScale+','+currentScale+')', /* Chrome, Safari, Opera */
            		'transform': 'scale('+currentScale+','+currentScale+')'
            	});
            } 


            /**
             *Get the workflow Object
             *@method getWorkflow A public method
             *@return {Object} workflow Current workflow object with details such as
             /* State transitions, State names, State positions and workflow container Id
             */
            instance.getWorkflow = function() {

                // Get updates array of State elements.
                windows = jsPlumb.getSelector("." + workflow + " .w");

                var workflowObject = {};

                workflowObject['transitions'] = instance.getStateTransitions();
                workflowObject['names'] = instance.getStateNames();
                workflowObject['positions'] = instance.getStatePositions();
                workflowObject['container'] = workflow;

                return workflowObject;
            }
            
            instance.getSplitJoinType = function(sj, type) {
            	if (type == null || type == undefined || type == '') {
            		return ' '+sj+'-no';
            	}
            	var res = 'no';
            	
            	if (type == 'v-wf:XOR')  return ' '+sj+'-xor';
            	if (type == 'v-wf:OR')   return ' '+sj+'-or';
            	if (type == 'v-wf:AND')  return ' '+sj+'-and';
            	if (type == 'v-wf:NONE') return ' '+sj+'-none';
            	
            	return ' '+sj+'-'+res;
            }
            
            // Add State to Workspace
            instance.createState = function(type, state) {
            	var stateElement = '';
            	switch (type) {
    			case 'v-wf:InputCondition':    				
    				stateElement = '<div class="w state-condition" id="' + state.id + '" style="font-size:20px;padding-top:10px;left: ' + (canvasSizePx/2+state['v-wf:locationX'][0]) + 'px; top: ' + (canvasSizePx/2+state['v-wf:locationY'][0]) + 'px;"><div><span class="glyphicon glyphicon-play" aria-hidden="true"></div><div class="ep"></div></div>';
    				break;
    			case 'v-wf:OutputCondition':
    				stateElement = '<div class="w state-condition" id="' + state.id + '" style="font-size:20px;padding-top:10px;left: ' + (canvasSizePx/2+state['v-wf:locationX'][0]) + 'px; top: ' + (canvasSizePx/2+state['v-wf:locationY'][0]) + 'px;"><div><span class="glyphicon glyphicon-stop" aria-hidden="true"></div></div>';
    				break;
    			case 'v-wf:Condition':
    				stateElement = '<div class="w state-condition" id="' + state.id + '" style="left: ' + (canvasSizePx/2+state['v-wf:locationX'][0]) + 'px; top: ' + (canvasSizePx/2+state['v-wf:locationY'][0]) + 'px;"><div class="state-name"></div><div class="ep"></div></div>';
    				break;
    			case 'v-wf:Task':    				
            		stateElement = '<div class="w state-task split-join '
            			+ instance.getSplitJoinType('split', (state['v-wf:split']!=null && state['v-wf:split'].length>0)?state['v-wf:split'][0].id:null)
            			+ instance.getSplitJoinType('join', (state['v-wf:join']!=null && state['v-wf:join'].length>0)?state['v-wf:join'][0].id:null)
            			+ '" id="' + state.id + '" style="left: ' 
            			+ (canvasSizePx/2+state['v-wf:locationX'][0]) + 'px; top: ' 
            			+ (canvasSizePx/2+state['v-wf:locationY'][0]) + 'px;"><div class="state-name">' + state['rdfs:label'][0] + '</div><div class="ep"></div></div>';
    				break;
    			}
            	if (stateElement!='') {
                	$('#'+workflowData).append(stateElement);
                	bindStateEvents($('#' + veda.Util.escape4$(state.id)));
                	updateSVGBackground($('#' + veda.Util.escape4$(state.id)));
            	}
            }
            
            instance.deleteState = function(element) {
            	instance.detachAllConnections(element);
            	instance.remove(element);
            	net['v-wf:consistsOf'] = net['v-wf:consistsOf'].filter(function (el) {if (el.id!=element.id) return el; });
            }
            
            instance.createFlow = function(state, flow) {
            	var connector = instance.connect({
            		id: flow.id,
                    source: state.id,
                    target: flow['v-wf:flowsInto'][0].id
                });
            }
            
            /**
             *Create workflow Net by given Object (v-wf:Net individual).
             *@method createNet A public method
             *@param {Object} workflowData A workflow object to create State transitions
             */
            instance.createNet = function(net) {
            	$('#workflow-net-name').text(net['rdfs:label'][0]);
            	// Create States
            	net['v-wf:consistsOf'].forEach(function(el) {
            		el['rdf:type'].forEach(function (type) {
            			instance.createState(type.id, el);
            		});
            	});
            	
            	// Create Flows
            	net['v-wf:consistsOf'].forEach(function(el) {
            		if (undefined != el['v-wf:hasFlow']) {
            			el['v-wf:hasFlow'].forEach(function (flow) {
            				instance.createFlow(el, flow);
            			});
            		}
            	});
            }

            instance.createNet(net);
                        
            if (localStorage.getItem("workflow"+net.id+"-zoom")>0 && localStorage.getItem("workflow"+net.id+"-zoom")!=1) {
            	instance.changeScale(localStorage.getItem("workflow"+net.id+"-zoom"));	
            }
            
            /* CONTEXT MENU [BEGIN] */
            var $contextMenu = $("#workflow-context-menu");
            
            $(".state-task").on("contextmenu", function(e) {
            	var _this = this,
            	    menu = $("#workflow-context-menu ul");
            	menu.html('');
            	getSubIndividual(net, 'v-wf:consistsOf', _this.id, function (el) {
            	   el['v-wf:startingMapping'].forEach(function(var_map) {
                	   var varMap = new veda.IndividualModel(var_map.id);
                 	   menu.append('<li><a href="#/document/'+varMap.id+'///edit">MAP : '+varMap.id+'</a></li>');
            	       var found = false;
            	       
            	       varMap['v-wf:mapsTo'].forEach(function(var_var) {
           	    		   var variable = new veda.IndividualModel(var_var.id);
           	    		   menu.append('<li><a href="#/document/'+var_var.id+'///edit">VAR : '+((variable['v-wf:variableName']!=null && variable['v-wf:variableName'].length>0)?variable['v-wf:variableName'][0]:var_var.id)+'</a></li>');
           	    		   found = true;
        	    	   });
                   });
                   el['v-wf:executor'].forEach(function(el2) {
                	   var variable = new veda.IndividualModel(el2.id);
                       menu.append('<li><a href="#/document/'+el2.id+'///edit">EXECUTOR : '+el2.id+'</a></li>');                       
                   });                                      
           	 	});
            	// 
            	   $contextMenu.css({
            	      display: "block",
            	      left: e.pageX-((e.pageX+$contextMenu.width()>$( document ).width())?$contextMenu.width():0),
            	      top: e.pageY-((e.pageY+$contextMenu.height()>$( document ).height())?$contextMenu.height():0)
            	   });
            	   return false;
            	});
            
            /* CONTEXT MENU [END]*/
            
            /* NET MENU [BEGIN] */
            $('#workflow-save-button').on('click', function() {
            	net.save();
            	net['v-wf:consistsOf'].forEach(function(el) {
            		el.save();
            	});
            });
            
            $('#workflow-export-ttl').on('click', function() {
           		var list = new veda.IndividualListModel(net, net['v-wf:consistsOf']);
           		veda.Util.exportTTL(list);
            });
            
            $('.delete-state').on('click', function() {
                deleteState = confirm('Deleting State(' + $('#workflow-item-id').val() + ') ...');

                if (deleteState) {            	
                	instance.deleteState(instance.getSelector('#'+veda.Util.escape4$($('#workflow-item-id').val()))[0]);
                }
            });
            
            /* ZOOM [BEGIN] */
            $('.zoom-in').on('click', function() {
            	if (currentScale<1) return instance.changeScale(currentScale + 0.1);
            	if (currentScale<2) return instance.changeScale(currentScale + 0.25);
            });

            $('.zoom-out').on('click', function() {
            	if (currentScale>1) return instance.changeScale(currentScale - 0.25);
            	if (currentScale>0.2) return instance.changeScale(currentScale - 0.1);
            });
            
            $('#'+workflowData).bind('mousewheel', function(e){
            	if(e.originalEvent.wheelDelta > 0) {
            		if (currentScale<1) return instance.changeScale(currentScale + 0.1);
                   	if (currentScale<2) return instance.changeScale(currentScale + 0.25);
                } else {
                    if (currentScale>1) return instance.changeScale(currentScale - 0.25);
                    if (currentScale>0.2) return instance.changeScale(currentScale - 0.1);
                }
            });
            
            $('.zoom-default').on('click', function() {
            	instance.changeScale(1);
            });
            /* ZOOM [END] */

            /* NET MENU [END] */
            
            return instance;
        }
    });
})();

//[END] Block of net editor
