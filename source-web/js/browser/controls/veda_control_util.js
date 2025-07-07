// Control utilities

import veda from '../../common/veda.js';

import IndividualModel from '../../common/individual_model.js';

import Util from '../../common/util.js';

import Backend from '../../common/backend.js';

export {ftQuery, ftQueryWithDeleted, storedQuery, renderValue, interpolate, convertToCyrillic, sanitizeInput};

/**
 * Perform full text search query
 * @param {string} prefix - The prefix for the query.
 * @param {string} input - The input text to search for.
 * @param {string} sort - The sorting order of the results.
 * @param {string} queryPattern - The query pattern with '{}' as a placeholder for user input.
 * @return {Promise} - A promise that resolves to the search results.
 */
function ftQuery (prefix, input, sort, queryPattern = "'*'=='{}'") {
  input = input ? input.trim() : '';
  let queryString = '';

  if (input) {
    const lines = input.split('\n').filter(Boolean);
    const lineQueries = lines.map((line) => {
      const special = line && line.indexOf('==') > 0 ? line : false;
      if (special) {
        return special;
      }
      const words = line.trim().replace(/[-*\s'"]+/g, ' ').split(' ').filter(Boolean);
      return words.map((word) => {
        return queryPattern.replaceAll('{}', word + '*');
      }).join(' && ');
    });
    queryString = lineQueries.filter(Boolean).join(' || ');
  }

  if (prefix) {
    queryString = queryString ? '(' + prefix + ') && (' + queryString + ')' : '(' + prefix + ')';
  }

  return incrementalSearchRegular(queryString, sort)
    .then((results) => {
      results = Util.unique( results );
      return Backend.get_individuals({
        ticket: veda.ticket,
        uris: results,
      });
    })
    .then((individuals) => Promise.all(individuals.map((individual) => new IndividualModel(individual).init())));
}

/**
 * Perform full text search query including deleted items
 * @param {string} prefix - The prefix for the query.
 * @param {string} input - The input text to search for.
 * @param {string} sort - The sorting order of the results.
 * @param {string} queryPattern - The query pattern with '{}' as a placeholder for user input.
 * @return {Promise} - A promise that resolves to the search results.
 */
function ftQueryWithDeleted (prefix, input, sort, queryPattern = "'*'=='{}'") {
  input = input ? input.trim() : '';
  let queryString = '';

  if (input) {
    const lines = input.split('\n').filter(Boolean);
    const lineQueries = lines.map((line) => {
      const special = line && line.indexOf('==') > 0 ? line : false;
      if (special) {
        return special;
      }
      const words = line.trim().replace(/[-*\s'"]+/g, ' ').split(' ').filter(Boolean);
      return words.map((word) => {
        return queryPattern.replaceAll('{}', word + '*');
      }).join(' && ');
    });
    queryString = lineQueries.filter(Boolean).join(' || ');
  }

  if (prefix) {
    queryString = queryString ? '(' + prefix + ') && (' + queryString + ')' : '(' + prefix + ')';
  }

  return incrementalSearchRegular(queryString, sort)
    .then((results) => {
      queryString = queryString + ' && (\'v-s:deleted\' == true )';
      return incrementalSearchRegular(queryString, sort, results);
    })
    .then((results) => {
      results = Util.unique( results );
      return Backend.get_individuals({
        ticket: veda.ticket,
        uris: results,
      });
    })
    .then((individuals) => Promise.all(individuals.map((individual) => new IndividualModel(individual).init())));
}

/**
 * Perform full text search query incrementally
 * @param {string} queryString
 * @param {string} sort
 * @param {Array} results
 * @param {string} cursor
 * @param {string} limit
 * @return {Promise}
 */
function incrementalSearchRegular (queryString, sort, results = [], cursor = 0, limit = 100) {
  const sortSuffix = (veda.user.getLanguage()[0] ? '_' + veda.user.getLanguage()[0].toLowerCase() : '');
  return Backend.query({
    ticket: veda.ticket,
    query: queryString,
    sort: sort || "'rdfs:label" + sortSuffix + "' asc",
    from: cursor,
    top: 10,
    limit: 1000,
  }).then((queryResult) => {
    results = results.concat(queryResult.result);
    const resultCursor = queryResult.cursor;
    const resultEstimated = queryResult.estimated;
    if (results.length >= limit || resultCursor >= resultEstimated) {
      return Promise.resolve(results);
    } else {
      return Promise.resolve(incrementalSearchRegular(queryString, sort, results, resultCursor, limit));
    }
  });
}

async function storedQuery (query, input = '', sort = '') {
  const queryParams = new IndividualModel();
  queryParams.set('rdf:type', 'v-s:QueryParams');
  queryParams.set('v-s:storedQuery', query);
  queryParams.set('v-s:resultFormat', 'cols');
  queryParams.set('v-s:content', '%' + input + '%');

  const allData = await Backend.stored_query(veda.ticket, queryParams.properties);
  if (allData.id) {
    return allData.id.map((id) => new IndividualModel(id));
  } else {
    throw new Error('Bad stored query output, no id column');
  }
}

function sanitizeInput (input) {
  return input.replace(/[\n\r\t]+/g, ' ')
    .replace(/[^a-zA-Z0-9а-яА-ЯёЁ@. +\-()]/g, '')
    .replace(/\s+/g, ' ')
    .trim();
}

/**
 * Render option value
 * @param {IndividualModel|string|number|Boolean|Date} value
 * @param {string} template
 * @return {Promise<string>}
 */
function renderValue (value, template) {
  if (value instanceof IndividualModel) {
    return value.load().then(() => {
      if (template) {
        return Promise.resolve(interpolate(template, value));
      } else {
        return value.hasValue('rdfs:label')
          ? Promise.resolve(value.get('rdfs:label').map(Util.formatValue).join(' '))
          : Promise.resolve(value.id);
      }
    });
  } else {
    return Promise.resolve(Util.formatValue(value));
  }
}

/**
 * Interpolate string for rendering
 * @param {string} template
 * @param {IndividualModel} individual
 * @return {Promise}
 */
function interpolate (template, individual) {
  const promises = [];
  const re_interpolate = /{\s*(.*?)\s*}/g;
  const re_evaluate = /{{\s*(.*?)\s*}}/g;
  template.replace(re_evaluate, (match, group) => {
    const rendered = eval(group);
    promises.push(Promise.resolve(String(rendered)));
    return '';
  }).replace(re_interpolate, (match, group) => {
    const properties = group.split('.');
    let target = properties.shift();
    if (target === '@') {
      target = individual;
    } else {
      target = new IndividualModel(target);
    }
    const rendered = target.getChainValue(...properties).then((values) => values.map(Util.formatValue).filter(Boolean).join(' '));
    promises.push(rendered);
    return '';
  });
  return Promise.all(promises).then((fulfilled) => {
    return template.replace(re_evaluate, () => fulfilled.shift()).replace(re_interpolate, () => fulfilled.shift());
  });
}

/**
 * Convert text from latin symblos to cyrillic
 * @param {string} text
 * @return {string} converted text
 */
function convertToCyrillic (text) {
  const cyrillic = 'йцукенгшщзхъфывапролджэячсмитьбюё';
  const latin = 'qwertyuiop[]asdfghjkl;\'zxcvbnm,.`';
  return text.toLowerCase().split('').map((char) => {
    const index = latin.indexOf(char);
    return index >= 0 ? cyrillic[index] : char;
  }).join('');
};
