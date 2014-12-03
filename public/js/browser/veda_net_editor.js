/**
 * NET EDITOR
 * 
 * Inspired by http://github.com/hemantsshetty/jsWorkflow
 */

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
        jsWorkflow.Instance.prototype.init = function(workflowData) {

            var instance,
                    windows,
                    addNewState,
                    bindStateEvents,
                    workflow;

            if (typeof workflowData === 'object') {
                workflow = workflowData.container;
                jsWorkflow.Instance.createWorkflowDOM(workflowData);
            } else {
                workflow = workflowData;
            }

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
                instance.detach(transition);
            });

            // Get an array of State elements.
            windows = jsPlumb.getSelector("#" + workflow + " .w");

            // Get a reference to the element in the workflow used to create a new State on click.
            addNewState = jsPlumb.getSelector(".create-state");

            /**
             *Bind required functionalities to State elements
             *@method bindStateEvents
             *@param {Object} windows List of all State elements
             */
            bindStateEvents = function(windows) {

                // По клику переходим на свойства объекта
                windows.bind("click", function() {
                    var _this = this, properties;
                    properties = $('#selected-item');
                    
                    properties.find('#item-id').val(_this.id);
                    
                    $('#'+properties.find('#item-id').val()).removeClass('w_active'); // deactivate old selection
                    ["no", "and", "or", "xor"].forEach(function(entry) {
                        if ($(_this).hasClass('split-'+entry)) {
                        	$('#split-buttons label').removeClass('active').find('input').attr("checked",false);                        	
                        	$('input[name=item-split-type][value='+entry+']').attr("checked",true).parent().addClass('active');
                        }                        
                        if ($(_this).hasClass('split-'+entry)) {
                        	$('#join-buttons label').removeClass('active').find('input').attr("checked",false);                        	
                        	$('input[name=item-join-type][value='+entry+']').attr("checked",true).parent().addClass('active');
                        }
                    });                    
                    /*
                    properties.find('#item-label').val("124124");
                    */
                    $(_this).addClass('w_active');
                });

                // Bind a click listener to each State elements. On double click, State elements are deleted.
                windows.bind("dblclick", function() {

                    var _this = this,
                            deleteState;

                    deleteState = confirm('Deleting State(' + jQuery(_this).attr('id').toUpperCase() + ') ...');

                    if (deleteState) {

                        // remove all the connections of this State element.
                        instance.detachAllConnections(_this);

                        // remove the State element.
                        jQuery(_this).remove();

                    } else {
                        return false;
                    }
                });

                // Initialize State elements as draggable.  
                instance.draggable(windows, {
              	  containment:"parent"
            	});

                // Initialize all State elements as Connection sources.
                instance.makeSource(windows, {
                    filter: ".ep",
                    anchor: ["Perimeter", { shape:"Rectangle" }],
                    connector: ["Straight", {
                            stub: 0,
                            gap: 1
                        }],
                    connectorStyle: {
                        strokeStyle: "#bbb",
                        lineWidth: 1,
                        outlineColor: "transparent",
                        outlineWidth: 4
                    },
                    /*
                    connectorOverlays: [
                        ["Arrow", {width: 10, length: 30, location: 1, id: "arrow"}],
                        ["Label", {label: "foo", id: "label"}]
                    ],*/
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
                    anchor: ["Perimeter", { shape:"Rectangle" }]
                });
            };

            // Initiate bindStateEvents on States elements present in the DOM
            if (windows.length > 0) {
                bindStateEvents(windows);
            }

            // Add new State event.
            addNewState.bind("click", function() {
                var _this = this,
                        stateName,
                        stateId,
                        stateElement;

                stateName = prompt("Enter the name of the state");

                if (stateName && stateName !== '') {

                    stateName = stateName.replace(/[^a-zA-Z0-9 ]/g, '');

                    stateId = stateName.toLocaleLowerCase().replace(' ', '-');

                    if (jQuery("#workflow-canvas").find('#' + stateId).length < 1) {

                        stateElement = '<div class="w wwww state split-join split-no join-no" id="' + stateId + '">' + stateName + '<div class="ep"></div></div>';

                        jQuery("#workflow-canvas").append(stateElement);

                        // Bind required functionalities to this State element
                        bindStateEvents(jQuery('#' + stateId));

                    } else {
                        alert('This state is already present.');
                    }
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

                    workflowStateName[jQuery(windows[i]).attr('id')] = jQuery(windows[i]).text().trim();
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

                    workflowStatePosition[jQuery(windows[i]).attr('id')] = jQuery(windows[i]).position();
                }
                return workflowStatePosition;
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

            /**
             *Create workflow State transitions from the given Object.
             *@method createStateTrasitions A public method
             *@param {Object} workflowData A workflow object to create State transitions
             */
            instance.createStateTrasitions = function(workflowData) {

                var transitions = workflowData.transitions,
                        targetState;

                for (var name in transitions) {
                    targetState = transitions[name].split(',');
                    for (var i = 0; i < targetState.length; i += 1) {
                        instance.connect({
                            source: name,
                            target: targetState[i]
                        });
                    }
                }
            }

            // Create workflow State transitions from the given workflowData Object.
            if (typeof workflowData === 'object') {
                instance.createStateTrasitions(workflowData);
            }

            return instance;
        }
        /**
         *Create the workflow DOM from the given data.
         *@method createWorkflowDOM
         *@param {Object} workflowData A workflow object to render new workflow State elements in the DOM
         */
        jsWorkflow.Instance.createWorkflowDOM = function(workflowData) {

            var container = workflowData.container,
                    names = workflowData.names,
                    positions = workflowData.positions,
                    elements = '';

            for (var name in names) {
                if (names.hasOwnProperty(name)) {
                    elements += '<div class="w wwww state split-join" id=' + name + ' style="left: ' + positions[name]['left'] + 'px; top: ' + positions[name]['top'] + 'px;">' + names[name] + '<div class="ep"></div></div>';
                }
            }

            jQuery('#' + container).append(elements);
        }
    });
})();

//[END] Block of net editor

// [BEGIN] Block of element editor

function updateSVGBackground(item) {
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

function applyNetEditorFunctions() {

  // Split type
  $("input[name=item-split-type]:radio").change(function () {
    var item = $('#' + $('#item-id').val());    
    item.removeClass('split-no split-and split-or split-xor');
    item.addClass('split-' + $(this).val());
    updateSVGBackground(item);
  });

  // Join type
  $("input[name=item-join-type]:radio").change(function () {
    var item = $('#' + $('#item-id').val());
    item.removeClass('join-no join-and join-or join-xor');
    item.addClass('join-' + $(this).val());
    updateSVGBackground(item);
  });
}

// [END] Block of element editor