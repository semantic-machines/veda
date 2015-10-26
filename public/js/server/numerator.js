"use strict";

/**
 * Numerator - auto increment exclusive value.
 * 
 * 1. Basic concepts:
 * 
 * 	  Scope - range of individuals, among that range numerator value must be unique
 *    Commit - add numerator value to scope 
 *    Revoke - remove numerator value from scope
 *    Rule - (see `v-s:NumerationRule`) rule for auto numeration specified by properties:
 *    + v-s:numeratorScope - script, return name of scope for specified individual
 *    + v-s:numeratorNextValue - script, returns next uncommitted value from rule scope. Returns fist value if scope is empty.
 *
 * 2. How it works
 *    
 *    2.1 You must specify `v-s:hasNumeratorRule` value for your individual property  
 * 
 * 	  2.2 Add `type="numerator"` property to your `<veda-control>` at your template. 
 * 	
 * 	  2.3 You can use already existed rule for v-ui:hasNumeratorRule. Or create your own. See examples : `v-s:SimpleNumerator`, `v-s:YearNumerator`
 *    
 *    2.4 Now when you click "Get number" button you get next uncommited value for you numeration scope. 
 *        Another way - you can type a number and it will be checked for uniqueness
 */
 
function numerate(ticket, individual, oldstate, _event_id) {
  try {	
	for (var key in individual) {
		var property = get_individual(ticket, key);
		if (property['v-s:hasNumeratorRule']) {
		  if (individual[key][0].data > 0) {	
			//print ('4'+key);
			var numerator = get_individual(ticket, property['v-s:hasNumeratorRule'][0].data);
			var scopeId = getScope(ticket, individual, numerator); 
			//print ('scopeId'+scopeId);
			var scope = get_individual(ticket, scopeId);
			
			if (typeof scope === "undefined") {
				scope = createScope(ticket, scopeId);
			}
			//print ('5'+scopeId);
			
			if (oldstate === "undefined" || !(oldstate[key] && oldstate[key][0].data > 0)) {
				if (commitValue(ticket, scope, parseInt(individual[key][0].data), _event_id)) {
					return {'sucess': true , 'result':'VALUE IS COMMITED'};
				} else {
					// TODO REFACTOR THIS
					individual[key][0].data = 0;
					put_individual(ticket, individual, _event_id);
					return {'sucess': false , 'result':'INVALID VALUE'};
				}
			} else {
				var oldScopeId = getScope(ticket, oldstate, numerator);
				//print (toJson(oldstate));
				if (scopeId == oldScopeId && individual[key][0].data == oldstate[key][0].data
					&& !(individual['v-s:deleted'].data =='true' && oldstate['v-s:deleted'].data == 'false')) {
					// scope and numbers are not changed
					if (individual['v-s:deleted'].data =='false' && oldstate['v-s:deleted'].data == 'true') {
						// document deleted
						revoke(ticket, scope, parseInt(individual[key][0].data), _event_id);
						return {'sucess': true, 'result':'VALUE REVOKED'};
					} else {
						return {'sucess': true, 'result':'NO CHANGES'};
					}
				} else {
					// commit in new scope, then revoke in old scope
					if (commitValue(ticket, scope, parseInt(individual[key][0].data), _event_id)) {
						var oldScope = get_individual(ticket, oldScopeId);
						revokeValue(ticket, oldScope, parseInt(oldstate[key][0].data), _event_id);
						return {'sucess': true , 'result':'VALUE IS COMMITED'};
					} else {
						// TODO REFACTOR THIS
						individual[key][0].data = 0;
						put_individual(ticket, individual, _event_id);
						return {'sucess': false , 'result':'INVALID VALUE'};
					}
				}
			}
		  } else {
			// TODO REFACTOR THIS
			individual[key][0].data = 0;
			put_individual(ticket, individual, _event_id);
			return {'sucess':false , 'result':'NUMBER IS NOT SPECIFIED'};
		  }
		} 
	} 
  } catch(e) {	
	print(e.stack);
  }
}

function getScope(ticket, individual, numerator) {
  try {		
	return eval(numerator['v-s:numeratorScope'][0].data)(ticket, individual);
  } catch(e) {	
	print(e.stack);
  }
}

function createScope(ticket, scopeId) {
  try {		
    var scope = {
        '@': scopeId,
        'rdfs:label' : [{data: scopeId, type: _String}],
        'rdf:type': [{
        	data: 'v-s:NumerationScope',
            type: _Uri
        }]
    };
	put_individual(ticket, scope, _event_id);
	return scope;
  } catch(e) {	
		print(e.stack);
  }
}

function commitValue(ticket, scope, value, _event_id) {
  try {		
	var nextInterval = null;
	var prevInterval = null;
	if (scope['v-s:numeratorCommitedInterval']) {
		// Scope is not empty
		for (var i in scope['v-s:numeratorCommitedInterval']) {
			var intervalUri = scope['v-s:numeratorCommitedInterval'][i];
			//print('commit0 >> '+intervalUri+' + '+intervalUri.data);
			var interval = get_individual(ticket, intervalUri.data);
			//print('commit1 >> '+toJson(scope));
			//print('commit2 >> '+toJson(interval));
			if ((interval['v-s:numeratorCommitedIntervalBegin'][0].data <= value) &&
				    (value <= interval['v-s:numeratorCommitedIntervalEnd'][0].data)) {
				// value is already commited
				return false;
			} else if (interval['v-s:numeratorCommitedIntervalBegin'][0].data == (value+1)) {
				nextInterval = interval;
			} else if (interval['v-s:numeratorCommitedIntervalEnd'][0].data == (value-1)) {
				prevInterval = interval;
			}
		}
		if (prevInterval != null && nextInterval != null) {
			// merge prev && value && next
			//print('merge prev && value && next');
			
			// prev = prev+next
			prevInterval['rdfs:label'][0].data = prevInterval['v-s:numeratorCommitedIntervalBegin'][0].data+' - '+nextInterval['v-s:numeratorCommitedIntervalEnd'][0].data;
			prevInterval['v-s:numeratorCommitedIntervalEnd'][0].data = nextInterval['v-s:numeratorCommitedIntervalEnd'][0].data
			put_individual(ticket, prevInterval, _event_id);
			
			// remove next 
			add_to_individual(ticket, {'@': nextInterval['@'],  'v-s:deleted': [{data:'true', type: _Bool }]}, false);
			var intervals = [];
			for (var i in scope['v-s:numeratorCommitedInterval']) {
				var intervalUri = scope['v-s:numeratorCommitedInterval'][i];
				if (intervalUri.data != nextInterval['@']) {
					intervals.push(intervalUri);
				}
			}
			scope['v-s:numeratorCommitedInterval'] = intervals;
			put_individual(ticket, scope, _event_id);
			
		} else if (prevInterval != null) {
			// merge prev && value
			//print('merge prev && value');
			prevInterval['rdfs:label'][0].data = prevInterval['v-s:numeratorCommitedIntervalBegin'][0].data+' - '+value;
			prevInterval['v-s:numeratorCommitedIntervalEnd'][0].data = value;
			put_individual(ticket, prevInterval, _event_id);
			
		} else if (nextInterval != null) {
			// merge value && next
			//print('merge value && next');
			nextInterval['rdfs:label'][0].data = value+' - '+nextInterval['v-s:numeratorCommitedIntervalEnd'][0].data;
			nextInterval['v-s:numeratorCommitedIntervalBegin'][0].data = value;
			put_individual(ticket, nextInterval, _event_id);
			
		} else {
			// new interval
			//print('new interval');
			
			var intervalId = genUri();
			var interval = {
				'@': intervalId,
		        'rdfs:label' : [{data: value+' - '+value, type: _String}],				
				'rdf:type' : [{data: 'v-s:NumerationCommitedInterval', type: _Uri }],
				'v-s:numeratorCommitedIntervalBegin'	 : [{data: value, type: _Integer}],
				'v-s:numeratorCommitedIntervalEnd'		 : [{data: value, type: _Integer}]
			}
			put_individual(ticket, interval, _event_id);
			
			scope['v-s:numeratorCommitedInterval'].push({data:interval['@'], type: _Uri});
			put_individual(ticket, scope, _event_id);
		}
	} else {
		// Scope is empty - create new interval
		var intervalId = genUri();
		var interval = {
			'@': intervalId,
			'rdfs:label' : [{data: value+' - '+value, type: _String}],
			'rdf:type' : [{data: 'v-s:NumerationCommitedInterval', type: _Uri }],
			'v-s:numeratorCommitedIntervalBegin'	 : [{data: value, type: _Integer}],
			'v-s:numeratorCommitedIntervalEnd'		 : [{data: value, type: _Integer}]
		}
		//print('Scope is empty - create new interval'+toJson(interval));
		put_individual(ticket, interval, _event_id);
		
		scope['v-s:numeratorCommitedInterval'] = [{data:interval['@'], type: _Uri}];
		put_individual(ticket, scope, _event_id);
	}	
	return true;
  } catch(e) {	
		print(e.stack);
  }
}

function revokeValue(ticket, scope, value, _event_id) {
  try {		
	var intervals = [];
	for (var i in scope['v-s:numeratorCommitedInterval']) {
		var intervalUri = scope['v-s:numeratorCommitedInterval'][i];
		var interval = get_individual(ticket, intervalUri.data);
		//print('revoke '+toJson(interval));
		if (interval['v-s:numeratorCommitedIntervalBegin'][0].data == value) {
			// value is an interval begin
			if (interval['v-s:numeratorCommitedIntervalBegin'][0].data < interval['v-s:numeratorCommitedIntervalEnd'][0].data) {
				// cut interval
				put_individual(ticket, {
					'@': interval['@'],
					'rdfs:label' : [{data: (value+1)+' - '+interval['v-s:numeratorCommitedIntervalEnd'][0].data, type: _String}],
					'rdf:type' : [{data: 'v-s:NumerationCommitedInterval', type: _Uri }],
					'v-s:numeratorCommitedIntervalBegin'	 : [{data: value+1, type: _Integer}],
					'v-s:numeratorCommitedIntervalEnd'		 : [{data: interval['v-s:numeratorCommitedIntervalEnd'][0].data, type: _Integer}]
				}, _event_id);
				intervals.push(intervalUri);
			} else {
				// remove empty interval
				add_to_individual(ticket, {'@': interval['@'], 'v-s:deleted': [{data:'true', type: _Bool }]}, false);
			} 
		} else if (interval['v-s:numeratorCommitedIntervalEnd'][0].data == value) {
			// value is an interval end
			if (interval['v-s:numeratorCommitedIntervalBegin'][0].data < interval['v-s:numeratorCommitedIntervalEnd'][0].data) {
				// cut interval
				put_individual(ticket, {
					'@': interval['@'],
					'rdfs:label' : [{data: interval['v-s:numeratorCommitedIntervalBegin'][0].data+' - '+(value-1), type: _String}],
					'rdf:type' : [{data: 'v-s:NumerationCommitedInterval', type: _Uri }],
					'v-s:numeratorCommitedIntervalBegin'	 : [{data: interval['v-s:numeratorCommitedIntervalBegin'][0].data, type: _Integer}],
					'v-s:numeratorCommitedIntervalEnd'		 : [{data: value-1, type: _Integer}]
				}, _event_id);
				intervals.push(intervalUri);
			} else {
				// remove empty interval
				add_to_individual(ticket, {'@': interval['@'], 'v-s:deleted': [{data:'true', type: _Bool }]}, false);
			}
		} else if ((interval['v-s:numeratorCommitedIntervalBegin'][0].data < value) &&
		    (value < interval['v-s:numeratorCommitedIntervalEnd'][0].data)) {
			// value strongly inside interval
			
			// cut current interval to value
			put_individual(ticket, {
				'@': interval['@'],
				'rdfs:label' : [{data: interval['v-s:numeratorCommitedIntervalBegin'][0].data+' - '+(value-1), type: _String}],
				'rdf:type' : [{data: 'v-s:NumerationCommitedInterval', type: _Uri }],
				'v-s:numeratorCommitedIntervalBegin'	 : [{data: interval['v-s:numeratorCommitedIntervalBegin'][0].data, type: _Integer}],
				'v-s:numeratorCommitedIntervalEnd'		 : [{data: value-1, type: _Integer}]
			}, _event_id);
			intervals.push(intervalUri);			
			
			// add new interval from value
			var newIntervalUri = {data:genUri(), type: _Uri}; 
			
			put_individual(ticket, {
				'@': newIntervalUri.data,
				'rdfs:label' : [{data: (value+1)+' - '+interval['v-s:numeratorCommitedIntervalEnd'][0].data, type: _String}],
				'rdf:type' : [{data: 'v-s:NumerationCommitedInterval', type: _Uri }],
				'v-s:numeratorCommitedIntervalBegin'	 : [{data: value+1, type: _Integer}],
				'v-s:numeratorCommitedIntervalEnd'		 : [{data: interval['v-s:numeratorCommitedIntervalEnd'][0].data, type: _Integer}]
			}, _event_id);			
			intervals.push(newIntervalUri);			
		} else {
			// value outside of interval
			intervals.push(intervalUri);
		}
	};
	
	scope['v-s:numeratorCommitedInterval'] = intervals;
	
	put_individual(ticket, scope, _event_id);
  } catch(e) {	
		print(e.stack);
  }
}