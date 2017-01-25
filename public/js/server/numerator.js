"use strict";

/**
 * Numeration - auto increment exclusive value.
 *
 * 1. Basic concepts:
 *
 * Scope - range of individuals, among that range numeration value must be
 * unique Commit - add numeration value to scope Revoke - remove numeration
 * value from scope Rule - (see `v-s:NumerationRule`) rule for auto numeration
 * specified by properties: + v-s:numerationScope - script, return name of scope
 * for specified individual + v-s:numerationNextValue - script, returns next
 * uncommitted value from rule scope. Returns fist value if scope is empty.
 *
 * 2. How it works
 *
 * 2.1 You must specify `v-s:hasNumerationMapper` values for your individual
 * property
 *
 * 2.2 Add one or may `v-s:NumerationMapper` with values: + v-s:numerationClass -
 * link to `owl:Class` - class that will be numerated against rule +
 * v-s:hasNumerationRule - link to `v-s:NumerationRule`
 *
 * 2.3 Add `type="numeration"` property to your `<veda-control>` at your
 * template.
 *
 * 2.4 You can use already existed rule for v-ui:hasNumerationRule. Or create
 * your own. See examples : `v-s:SimpleNumeration`, `v-s:YearNumeration`
 *
 * 2.5 add `rdfs:subClassOf v-s:Enumerated ;` to your class description to
 * trigger numeration event on server side.
 *
 * 2.6 Now when you click "Get number" button you get next uncommited value for
 * you numeration scope. Another way - you can type a number and it will be
 * checked for uniqueness
 */
function numerate(ticket, individual, super_classes, oldstate, _event_id) {
  try {
    // Do not numerate versions
    if (individual['v-s:actualVersion'] && individual['v-s:actualVersion'][0].data != individual['@']) return;

    for (var idx in individual['rdf:type']) {
      var type = get_individual(ticket, individual['rdf:type'][idx].data);

      if (type && type['v-s:hasNumeration']) {
        var rule = get_individual(ticket, type['v-s:hasNumeration'][0].data);
        var key = rule['v-s:enumeratedProperty'][0].data;

        if (individual[key] && individual[key][0].data > 0) {
          var numeration = get_individual(ticket, rule['v-s:hasNumerationRule'][0].data);
          var scopeId = getScope(ticket, individual, numeration);
          var scope = get_individual(ticket, scopeId);

          if (typeof scope === "undefined") {
            scope = createScope(ticket, scopeId);
          }
          if ( typeof oldstate === "undefined" || !(oldstate[key] && oldstate[key][0].data > 0) ) {
            if ( commitValue(ticket, scope, parseInt(individual[key][0].data), _event_id) ) {
              return {
                'sucess' : true,
                'result' : 'VALUE IS COMMITED'
              };
            } else {
              // TODO REFACTOR THIS
              //individual[key][0].data = 0;
              //put_individual(ticket, individual, _event_id);
              return {
                'sucess' : false,
                'result' : 'INVALID VALUE'
              };
            }
          } else {
            var oldScopeId = getScope(ticket, oldstate, numeration);
            if (
              scopeId == oldScopeId
              && individual[key][0].data == oldstate[key][0].data
              && (!individual['v-s:deleted'] || !oldstate['v-s:deleted'] || (!(individual['v-s:deleted'][0].data === true && oldstate['v-s:deleted'][0].data === false)))
            ) {
              // scope and numbers are not changed
              if (
                (!oldstate['v-s:deleted'] || oldstate['v-s:deleted'][0].data === false)
                && (individual['v-s:deleted'] && individual['v-s:deleted'][0].data === true)
              ) {
                // document deleted
                revokeValue(ticket, scope, parseInt(individual[key][0].data), _event_id);
                return {
                  'sucess' : true,
                  'result' : 'VALUE REVOKED'
                };
              } else {
                return {
                  'sucess' : true,
                  'result' : 'NO CHANGES'
                };
              }
            } else {
              // commit in new scope, then revoke in old scope
              if (commitValue(ticket, scope, parseInt(individual[key][0].data), _event_id)) {
                var oldScope = get_individual(ticket, oldScopeId);
                revokeValue(ticket, oldScope, parseInt(oldstate[key][0].data), _event_id);
                return {
                  'sucess' : true,
                  'result' : 'VALUE IS COMMITED'
                };
              } else {
                // TODO REFACTOR THIS
                //individual[key][0].data = 0;
                //put_individual(ticket, individual, _event_id);
                return {
                  'sucess' : false,
                  'result' : 'INVALID VALUE'
                };
              }
            }
          }
        } else {
          // TODO REFACTOR THIS
          //if (individual[key]) {
          //  individual[key][0].data = 0;
          //  put_individual(ticket, individual, _event_id);
          //}
          return {
            'sucess' : false,
            'result' : 'NUMBER IS NOT SPECIFIED'
          };
        }
      }
    }
  } catch (e) {
    print(e.stack);
  }
}

function getScope(ticket, individual, numeration) {
  try {
    return eval(numeration['v-s:numerationScope'][0].data)(ticket, individual);
  } catch (e) {
    print(e.stack);
  }
}

function createScope(ticket, scopeId) {
  try {
    var scope = {
      '@' : scopeId,
      'rdfs:label' : [{
        data : scopeId,
        type : _String
      }],
      'rdf:type' : [{
        data : 'v-s:NumerationScopeClass',
        type : _Uri
      }]
    };
    put_individual(ticket, scope, _event_id);
    return scope;
  } catch (e) {
    print(e.stack);
  }
}

function commitValue(ticket, scope, value, _event_id) {
  try {
    var nextInterval = null;
    var prevInterval = null;
    if (scope['v-s:numerationCommitedInterval']) {
      // Scope is not empty
      for ( var i in scope['v-s:numerationCommitedInterval']) {
        var intervalUri = scope['v-s:numerationCommitedInterval'][i];
        var interval = get_individual(ticket, intervalUri.data);
        if ( (interval['v-s:numerationCommitedIntervalBegin'][0].data <= value) && (value <= interval['v-s:numerationCommitedIntervalEnd'][0].data) ) {
          // value is already commited
          return false;
        } else if (interval['v-s:numerationCommitedIntervalBegin'][0].data == (value + 1)) {
          nextInterval = interval;
        } else if (interval['v-s:numerationCommitedIntervalEnd'][0].data == (value - 1)) {
          prevInterval = interval;
        }
      }
      if (prevInterval != null && nextInterval != null) {
        // merge prev && value && next
        // prev = prev+next
        prevInterval['rdfs:label'][0].data = prevInterval['v-s:numerationCommitedIntervalBegin'][0].data + ' - ' + nextInterval['v-s:numerationCommitedIntervalEnd'][0].data;
        prevInterval['v-s:numerationCommitedIntervalEnd'][0].data = nextInterval['v-s:numerationCommitedIntervalEnd'][0].data
        put_individual(ticket, prevInterval, _event_id);

        // remove next
        add_to_individual(ticket, {
          '@' : nextInterval['@'],
          'v-s:deleted' : [{
            data : 'true',
            type : _Bool
          }]
        }, false);
        var intervals = [];
        for ( var i in scope['v-s:numerationCommitedInterval']) {
          var intervalUri = scope['v-s:numerationCommitedInterval'][i];
          if (intervalUri.data != nextInterval['@']) {
            intervals.push(intervalUri);
          }
        }
        scope['v-s:numerationCommitedInterval'] = intervals;
        put_individual(ticket, scope, _event_id);

      } else if (prevInterval != null) {
        // merge prev && value
        prevInterval['rdfs:label'][0].data = prevInterval['v-s:numerationCommitedIntervalBegin'][0].data + ' - ' + value;
        prevInterval['v-s:numerationCommitedIntervalEnd'][0].data = value;
        put_individual(ticket, prevInterval, _event_id);

      } else if (nextInterval != null) {
        // merge value && next
        nextInterval['rdfs:label'][0].data = value + ' - ' + nextInterval['v-s:numerationCommitedIntervalEnd'][0].data;
        nextInterval['v-s:numerationCommitedIntervalBegin'][0].data = value;
        put_individual(ticket, nextInterval, _event_id);

      } else {
        // new interval
        var intervalId = genUri();
        var interval = {
          '@' : intervalId,
          'rdfs:label' : [{
            data : value + ' - ' + value,
            type : _String
          }],
          'rdf:type' : [{
            data : 'v-s:NumerationCommitedIntervalClass',
            type : _Uri
          }],
          'v-s:numerationCommitedIntervalBegin' : [{
            data : value,
            type : _Integer
          }],
          'v-s:numerationCommitedIntervalEnd' : [ {
            data : value,
            type : _Integer
          }]
        }
        put_individual(ticket, interval, _event_id);

        scope['v-s:numerationCommitedInterval'].push({
          data : interval['@'],
          type : _Uri
        });
        put_individual(ticket, scope, _event_id);
      }
    } else {
      // Scope is empty - create new interval
      var intervalId = genUri();
      var interval = {
        '@' : intervalId,
        'rdfs:label' : [{
          data : value + ' - ' + value,
          type : _String
        }],
        'rdf:type' : [{
          data : 'v-s:NumerationCommitedIntervalClass',
          type : _Uri
        }],
        'v-s:numerationCommitedIntervalBegin' : [{
          data : value,
          type : _Integer
        }],
        'v-s:numerationCommitedIntervalEnd' : [{
          data : value,
          type : _Integer
        }]
      }
      put_individual(ticket, interval, _event_id);

      scope['v-s:numerationCommitedInterval'] = [{
        data : interval['@'],
        type : _Uri
      }];
      put_individual(ticket, scope, _event_id);
    }
    return true;
  } catch (e) {
    print(e.stack);
  }
}

function revokeValue(ticket, scope, value, _event_id) {
  try {
    var intervals = [];
    for ( var i in scope['v-s:numerationCommitedInterval']) {
      var intervalUri = scope['v-s:numerationCommitedInterval'][i];
      var interval = get_individual(ticket, intervalUri.data);

      if (interval['v-s:numerationCommitedIntervalBegin'][0].data == value) {
        // value is an interval begin
        if (interval['v-s:numerationCommitedIntervalBegin'][0].data < interval['v-s:numerationCommitedIntervalEnd'][0].data) {
          // cut interval
          put_individual(
            ticket,
            {
              '@' : interval['@'],
              'rdfs:label' : [{
                data : (value + 1) + ' - ' + interval['v-s:numerationCommitedIntervalEnd'][0].data,
                type : _String
              }],
              'rdf:type' : [{
                data : 'v-s:NumerationCommitedIntervalClass',
                type : _Uri
              }],
              'v-s:numerationCommitedIntervalBegin' : [{
                data : value + 1,
                type : _Integer
              }],
              'v-s:numerationCommitedIntervalEnd' : [{
                data : interval['v-s:numerationCommitedIntervalEnd'][0].data,
                type : _Integer
              }]
            }, _event_id);
          intervals.push(intervalUri);
        } else {
          // remove empty interval
          add_to_individual(ticket, {
            '@' : interval['@'],
            'v-s:deleted' : [ {
              data : 'true',
              type : _Bool
            }]
          }, false);
        }
      } else if (interval['v-s:numerationCommitedIntervalEnd'][0].data == value) {
        // value is an interval end
        if (interval['v-s:numerationCommitedIntervalBegin'][0].data < interval['v-s:numerationCommitedIntervalEnd'][0].data) {
          // cut interval
          put_individual(
            ticket,
            {
              '@' : interval['@'],
              'rdfs:label' : [{
                data : interval['v-s:numerationCommitedIntervalBegin'][0].data + ' - ' + (value - 1),
                type : _String
              }],
              'rdf:type' : [{
                data : 'v-s:NumerationCommitedIntervalClass',
                type : _Uri
              }],
              'v-s:numerationCommitedIntervalBegin' : [{
                data : interval['v-s:numerationCommitedIntervalBegin'][0].data,
                type : _Integer
              }],
              'v-s:numerationCommitedIntervalEnd' : [{
                data : value - 1,
                type : _Integer
              }]
            },
            _event_id
          );
          intervals.push(intervalUri);
        } else {
          // remove empty interval
          add_to_individual(ticket, {
            '@' : interval['@'],
            'v-s:deleted' : [{
              data : 'true',
              type : _Bool
            }]
          }, false);
        }
      } else if ( (interval['v-s:numerationCommitedIntervalBegin'][0].data < value) && (value < interval['v-s:numerationCommitedIntervalEnd'][0].data) ) {
        // value strongly inside interval

        // cut current interval to value
        put_individual(
          ticket,
          {
            '@' : interval['@'],
            'rdfs:label' : [{
              data : interval['v-s:numerationCommitedIntervalBegin'][0].data + ' - ' + (value - 1),
              type : _String
            }],
            'rdf:type' : [{
              data : 'v-s:NumerationCommitedIntervalClass',
              type : _Uri
            }],
            'v-s:numerationCommitedIntervalBegin' : [{
              data : interval['v-s:numerationCommitedIntervalBegin'][0].data,
              type : _Integer
            }],
            'v-s:numerationCommitedIntervalEnd' : [{
              data : value - 1,
              type : _Integer
            }]
          },
          _event_id
        );
        intervals.push(intervalUri);

        // add new interval from value
        var newIntervalUri = {
          data : genUri(),
          type : _Uri
        };

        put_individual(
          ticket,
          {
            '@' : newIntervalUri.data,
            'rdfs:label' : [{
              data : (value + 1) + ' - ' + interval['v-s:numerationCommitedIntervalEnd'][0].data,
              type : _String
            }],
            'rdf:type' : [{
              data : 'v-s:NumerationCommitedIntervalClass',
              type : _Uri
            }],
            'v-s:numerationCommitedIntervalBegin' : [{
              data : value + 1,
              type : _Integer
            }],
            'v-s:numerationCommitedIntervalEnd' : [{
              data : interval['v-s:numerationCommitedIntervalEnd'][0].data,
              type : _Integer
            }]
          },
          _event_id
        );
        intervals.push(newIntervalUri);
      } else {
        // value outside of interval
        intervals.push(intervalUri);
      }
    }

    scope['v-s:numerationCommitedInterval'] = intervals;
    put_individual(ticket, scope, _event_id);

  } catch (e) {
    print(e.stack);
  }
}
