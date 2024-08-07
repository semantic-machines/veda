/**
 * Numeration - auto increment exclusive value.
 *
 * 1. Basic concepts:
 *
 * Scope - range of individuals, among that range numeration value must be unique.
 * Commit - add numeration value to scope.
 * Revoke - remove numeration value from scope.
 * Rule - (see `v-s:NumerationRule`) rule for auto numeration specified by properties:
 *  + v-s:numerationScope - script, return name of scope for specified individual.
 *  + v-s:numerationNextValue - script, returns next uncommitted value from rule scope.
 * Returns fist value if scope is empty.
 *
 * 2. How it works
 *
 * 2.1 Add `rdfs:subClassOf v-s:Enumerated ;` to your class description to
 * trigger numeration event on server side.
 * Define v-s:hasNumeration for a class. Individuals of this class will be numerated with defined numeration.
 *
 * 2.2 Chosen numeration must define v-s:enumeratedProperty & v-s:hasNumerationRule.
 * You can use already existing rule for v-ui:hasNumerationRule or create your own.
 * See examples: `v-s:SimpleNumeration`, `v-s:YearNumeration`.
 *
 * 2.3 Individuals of a mentioned class are automatically numerated with defined rule.
 * Numbers are stored in v-s:enumeratedProperty.
 */

import ServerUtil from '../server/util.js';

const Numerator = {};

export default Numerator;

Numerator.numerate = function (ticket, individual, super_classes, prev_state, _event_id) {
  try {
    const deleted = ServerUtil.hasValue( individual, 'v-s:deleted', {data: true, type: 'Boolean'} );
    const prevDeleted = prev_state && ServerUtil.hasValue( prev_state, 'v-s:deleted', {data: true, type: 'Boolean'} );

    individual['rdf:type'] && individual['rdf:type'].length && individual['rdf:type'].forEach((typeValue) => {
      const type = get_individual(ticket, typeValue.data);
      if (!type || !type['v-s:hasNumeration']) {
        return;
      }

      const numeration = get_individual(ticket, type['v-s:hasNumeration'][0].data);
      const enumeratedProperty = numeration['v-s:enumeratedProperty'][0].data;
      let number = parseInt( individual[enumeratedProperty] && individual[enumeratedProperty].length && individual[enumeratedProperty][0].data || 0 );
      const prevNumber = parseInt( prev_state && prev_state[enumeratedProperty] && prev_state[enumeratedProperty][0].data || 0 );

      // Already processed
      if (number && prevNumber && number === prevNumber && deleted === prevDeleted) {
        // Nothing changed return
        // print("@1 nothing changed exit");
        return;
      } else {
        const rule = get_individual(ticket, numeration['v-s:hasNumerationRule'][0].data);
        const scopeId = getScope(ticket, individual, rule);
        const scope = get_individual(ticket, scopeId) || createScope(ticket, scopeId);

        if (!number && !prevNumber) {
          // update doc, commit number
          number = getNewValue(ticket, individual, rule, scope);
          commitValue(ticket, scope, number, _event_id);
          individual[enumeratedProperty] = ServerUtil.newStr( number.toString() );
          put_individual(ticket, individual, _event_id);
        } else if (!number && prevNumber) {
          individual[enumeratedProperty] = ServerUtil.newStr( prevNumber.toString() ); // Restore number
          put_individual(ticket, individual, _event_id);
        } else if (number && !prev_state) {
          // commit number
          commitValue(ticket, scope, number, _event_id);
        } else if (number && deleted) {
          // revoke number
          revokeValue(ticket, scope, number, _event_id);
        } else if (number && prevNumber && number !== prevNumber) {
          // commit number, revoke prevNumber
          commitValue(ticket, scope, number, _event_id);
          const prevScopeId = getScope(ticket, prev_state, rule);
          const prevScope = get_individual(ticket, prevScopeId);
          revokeValue(ticket, prevScope, prevNumber, _event_id);
        }
      }
    });
  } catch (e) {
    print(e.stack);
  }
};

/**
 * Function to get new value
 *
 * @param {string} ticket
 * @param {Object} individual
 * @param {Object} rule
 * @param {string} scope
 * @return {string}
 */
function getNewValue (ticket, individual, rule, scope) {
  try {
    return eval(rule['v-s:numerationGetNextValue'][0].data)(ticket, scope);
  } catch (e) {
    print('getNewValue error', e.stack);
  }
}

/**
 * Function to get scope
 *
 * @param {string} ticket
 * @param {Object} individual
 * @param {Object} rule
 * @return {string}
 */
function getScope (ticket, individual, rule) {
  try {
    return eval(rule['v-s:numerationScope'][0].data)(ticket, individual);
  } catch (e) {
    print(e.stack);
  }
}

/**
 * Function to create new scope
 *
 * @param {string} ticket
 * @param {string} scopeId
 * @return {string}
 */
function createScope (ticket, scopeId) {
  try {
    const scope = {
      '@': scopeId,
      'rdfs:label': [{data: scopeId, type: 'String'}],
      'rdf:type': [{data: 'v-s:NumerationScopeClass', type: 'Uri'}],
    };
    put_individual(ticket, scope, _event_id);
    return scope;
  } catch (e) {
    print(e.stack);
  }
}

/**
 * Function to commit value to numerator
 *
 * @param {string} ticket
 * @param {string} scope
 * @param {string} value
 * @param {string} _event_id
 * @return {boolean}
 */
function commitValue (ticket, scope, value, _event_id) {
  try {
    let nextInterval = null;
    let prevInterval = null;
    if (scope['v-s:numerationCommitedInterval']) {
      // Scope is not empty
      for (const i in scope['v-s:numerationCommitedInterval']) {
        if (Object.hasOwnProperty.call(scope['v-s:numerationCommitedInterval'], i)) {
          const intervalUri = scope['v-s:numerationCommitedInterval'][i].data;
          const interval = get_individual(ticket, intervalUri);
          try {
            if ( (interval['v-s:numerationCommitedIntervalBegin'][0].data <= value) && (value <= interval['v-s:numerationCommitedIntervalEnd'][0].data) ) {
              // value is already commited
              return false;
            } else if (interval['v-s:numerationCommitedIntervalBegin'][0].data == (value + 1)) {
              nextInterval = interval;
            } else if (interval['v-s:numerationCommitedIntervalEnd'][0].data == (value - 1)) {
              prevInterval = interval;
            }
          } catch (err) {
            print('ERR! intervalUri=', intervalUri);
            print(err.stack);
          }
        }
      }
      if (prevInterval != null && nextInterval != null) {
        // merge prev && value && next
        // prev = prev+next
        prevInterval['rdfs:label'][0].data = prevInterval['v-s:numerationCommitedIntervalBegin'][0].data + ' - ' + nextInterval['v-s:numerationCommitedIntervalEnd'][0].data;
        prevInterval['v-s:numerationCommitedIntervalEnd'][0].data = nextInterval['v-s:numerationCommitedIntervalEnd'][0].data;
        put_individual(ticket, prevInterval, _event_id);

        // remove next
        add_to_individual(ticket, {
          '@': nextInterval['@'],
          'v-s:deleted': [{data: true, type: 'Boolean'}],
        }, false);
        const intervals = [];
        for (const i in scope['v-s:numerationCommitedInterval']) {
          if (Object.hasOwnProperty.call(scope['v-s:numerationCommitedInterval'], i)) {
            const intervalUri = scope['v-s:numerationCommitedInterval'][i];
            if (intervalUri.data != nextInterval['@']) {
              intervals.push(intervalUri);
            }
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
        const intervalId = ServerUtil.genUri() + '-intv';
        const interval = {
          '@': intervalId,
          'rdfs:label': [{data: value + ' - ' + value, type: 'String'}],
          'rdf:type': [{data: 'v-s:NumerationCommitedIntervalClass', type: 'Uri'}],
          'v-s:numerationCommitedIntervalBegin': [{data: value, type: 'Integer'}],
          'v-s:numerationCommitedIntervalEnd': [{data: value, type: 'Integer'}],
        };
        put_individual(ticket, interval, _event_id);

        scope['v-s:numerationCommitedInterval'].push( {data: interval['@'], type: 'Uri'} );
        put_individual(ticket, scope, _event_id);
      }
    } else {
      // Scope is empty - create new interval
      const intervalId = ServerUtil.genUri() + '-intv';
      const interval = {
        '@': intervalId,
        'rdfs:label': [{data: value + ' - ' + value, type: 'String'}],
        'rdf:type': [{data: 'v-s:NumerationCommitedIntervalClass', type: 'Uri'}],
        'v-s:numerationCommitedIntervalBegin': [{data: value, type: 'Integer'}],
        'v-s:numerationCommitedIntervalEnd': [{data: value, type: 'Integer'}],
      };
      put_individual(ticket, interval, _event_id);

      scope['v-s:numerationCommitedInterval'] = [{data: interval['@'], type: 'Uri'}];
      put_individual(ticket, scope, _event_id);
    }
    return true;
  } catch (e) {
    print(e.stack);
  }
}

/**
 * Function to revoke value from numerator
 *
 * @param {string} ticket
 * @param {string} scope
 * @param {string} value
 * @param {string} _event_id
 * @return {void}
 */
function revokeValue (ticket, scope, value, _event_id) {
  try {
    const intervals = [];
    for ( const i in scope['v-s:numerationCommitedInterval']) {
      if (Object.hasOwnProperty.call(scope['v-s:numerationCommitedInterval'], i)) {
        const intervalUri = scope['v-s:numerationCommitedInterval'][i];
        const interval = get_individual(ticket, intervalUri.data);

        if (interval['v-s:numerationCommitedIntervalBegin'][0].data == value) {
          // value is an interval begin
          if (interval['v-s:numerationCommitedIntervalBegin'][0].data < interval['v-s:numerationCommitedIntervalEnd'][0].data) {
            // cut interval
            put_individual(
              ticket,
              {
                '@': interval['@'],
                'rdfs:label': [{data: (value + 1) + ' - ' + interval['v-s:numerationCommitedIntervalEnd'][0].data, type: 'String'}],
                'rdf:type': [{data: 'v-s:NumerationCommitedIntervalClass', type: 'Uri'}],
                'v-s:numerationCommitedIntervalBegin': [{data: value + 1, type: 'Integer'}],
                'v-s:numerationCommitedIntervalEnd': [{data: interval['v-s:numerationCommitedIntervalEnd'][0].data, type: 'Integer'}],
              }, _event_id);
            intervals.push(intervalUri);
          } else {
            // remove empty interval
            add_to_individual(ticket, {
              '@': interval['@'],
              'v-s:deleted': [{data: true, type: 'Boolean'}],
            }, false);
          }
        } else if (interval['v-s:numerationCommitedIntervalEnd'][0].data == value) {
          // value is an interval end
          if (interval['v-s:numerationCommitedIntervalBegin'][0].data < interval['v-s:numerationCommitedIntervalEnd'][0].data) {
            // cut interval
            put_individual(
              ticket,
              {
                '@': interval['@'],
                'rdfs:label': [{data: interval['v-s:numerationCommitedIntervalBegin'][0].data + ' - ' + (value - 1), type: 'String'}],
                'rdf:type': [{data: 'v-s:NumerationCommitedIntervalClass', type: 'Uri'}],
                'v-s:numerationCommitedIntervalBegin': [{data: interval['v-s:numerationCommitedIntervalBegin'][0].data, type: 'Integer'}],
                'v-s:numerationCommitedIntervalEnd': [{data: value - 1, type: 'Integer'}],
              },
              _event_id,
            );
            intervals.push(intervalUri);
          } else {
            // remove empty interval
            add_to_individual(ticket, {
              '@': interval['@'],
              'v-s:deleted': [{data: true, type: 'Boolean'}],
            }, false);
          }
        } else if ( (interval['v-s:numerationCommitedIntervalBegin'][0].data < value) && (value < interval['v-s:numerationCommitedIntervalEnd'][0].data) ) {
          // value strongly inside interval

          // cut current interval to value
          put_individual(
            ticket,
            {
              '@': interval['@'],
              'rdfs:label': [{data: interval['v-s:numerationCommitedIntervalBegin'][0].data + ' - ' + (value - 1), type: 'String'}],
              'rdf:type': [{data: 'v-s:NumerationCommitedIntervalClass', type: 'Uri'}],
              'v-s:numerationCommitedIntervalBegin': [{data: interval['v-s:numerationCommitedIntervalBegin'][0].data, type: 'Integer'}],
              'v-s:numerationCommitedIntervalEnd': [{data: value - 1, type: 'Integer'}],
            },
            _event_id,
          );
          intervals.push(intervalUri);

          // add new interval from value
          const newIntervalUri = {data: ServerUtil.genUri() + '-intv', type: 'Uri'};

          put_individual(
            ticket,
            {
              '@': newIntervalUri.data,
              'rdfs:label': [{data: (value + 1) + ' - ' + interval['v-s:numerationCommitedIntervalEnd'][0].data, type: 'String'}],
              'rdf:type': [{data: 'v-s:NumerationCommitedIntervalClass', type: 'Uri'}],
              'v-s:numerationCommitedIntervalBegin': [{data: value + 1, type: 'Integer'}],
              'v-s:numerationCommitedIntervalEnd': [{data: interval['v-s:numerationCommitedIntervalEnd'][0].data, type: 'Integer'}],
            },
            _event_id,
          );
          intervals.push(newIntervalUri);
        } else {
          // value outside of interval
          intervals.push(intervalUri);
        }
      }
    }

    scope['v-s:numerationCommitedInterval'] = intervals;
    put_individual(ticket, scope, _event_id);
  } catch (e) {
    print(e.stack);
  }
}

/**
 * General function for getNextValue method for numerators
 *
 * @param {string} ticket
 * @param {string} scope - numerator scope
 * @param {string} FIRST_VALUE - first value in scope
 * @return {string}
 */
Numerator.getNextValueSimple = function (ticket, scope, FIRST_VALUE) {
  if (typeof scope === 'string') {
    try {
      scope = get_individual(ticket, scope);
    } catch (e) {
      return ''+FIRST_VALUE;
    }
  }
  if (typeof scope === 'undefined' || !scope['v-s:numerationCommitedInterval'] || scope['v-s:numerationCommitedInterval'].length === 0) {
    return ''+FIRST_VALUE;
  }
  let max = 0;
  scope['v-s:numerationCommitedInterval'].forEach((interval) => {
    const intervalUri = interval.data;
    try {
      interval = get_individual(ticket, intervalUri);
      if (interval['v-s:numerationCommitedIntervalEnd'][0].data > max) {
        max = interval['v-s:numerationCommitedIntervalEnd'][0].data;
      }
    } catch (err) {
      print('ERR! intervalUri = ', intervalUri);
      print(err.stack);
    }
  });
  return ''+(max + 1);
};
