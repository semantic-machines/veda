// Radio control

import $ from 'jquery';

import Util from '../../common/util.js';

import IndividualModel from '../../common/individual_model.js';

import {interpolate, ftQuery, ftQueryWithDeleted, renderValue} from './veda_control_util.js';

$.fn.veda_radio = function (params) {
  const opts = {...defaults, ...params};
  const self = this;
  const individual = opts.individual;
  const property_uri = opts.property_uri || opts.rel_uri;
  const spec = opts.spec;
  const rangeRestriction = spec && spec.hasValue('v-ui:rangeRestriction') ? spec['v-ui:rangeRestriction'][0] : undefined;
  const range = rangeRestriction ? [rangeRestriction] : (new IndividualModel(property_uri))['rdfs:range'];
  const queryPattern = this.attr('data-query-pattern') ?? (spec && spec.hasValue('v-ui:queryPattern') ? spec['v-ui:queryPattern'][0].toString() : undefined);
  const queryPrefix = this.attr('data-query-prefix') || ( spec && spec.hasValue('v-ui:queryPrefix') ? spec['v-ui:queryPrefix'][0] : range.map((item) => {
    return '\'rdf:type\'===\'' + item.id + '\'';
  }).join(' || ') );
  const sort = this.attr('data-sort') || ( spec && spec.hasValue('v-ui:sort') && spec['v-ui:sort'][0].toString() );
  const source = this.attr('data-source');
  const template = this.attr('data-template') || '{@.rdfs:label}';
  let withDeleted = this.attr('data-deleted') || false;

  populate();

  individual.on(property_uri, handler);
  this.one('remove', function () {
    individual.off(property_uri, handler);
  });

  if (template) {
    this.removeAttr('data-template');
  }

  /**
   * Populate options list
   * @return {Promise}
   */
  function populate () {
    if (spec && spec.hasValue('v-ui:optionValue')) {
      const options = spec['v-ui:optionValue'];
      return renderOptions(options);
    } else if (source) {
      return Promise.resolve(eval(source))
        .then(renderOptions)
        .catch((error) => {
          console.error('Source failed', source);
        });
    } else if (queryPrefix) {
      return interpolate(queryPrefix, individual)
        .then((prefix) => {
          return withDeleted ? ftQueryWithDeleted(prefix, undefined, sort, queryPattern) : ftQuery(prefix, undefined, sort, queryPattern);
        })
        .then(renderOptions)
        .catch((error) => {
          console.error('Query prefix failed', queryPrefix);
        });
    }
  }

  /**
   * Render options list
   * @param {Array} options
   * @return {void}
   */
  function renderOptions (options) {
    self.empty();
    return Promise.all(options.map((value, index) => {
      if (index >= 100) {
        return;
      }
      const hld = $(opts.template).appendTo(self);
      return renderValue(value, template).then((rendered) => {
        const lbl = $('label', hld).append( rendered );
        const rad = $('input', lbl).data('value', value);
        if (value instanceof IndividualModel && value.hasValue('v-s:deleted', true)) {
          hld.addClass('deleted');
        }
        const hasValue = individual.hasValue(property_uri, value);
        rad.prop('checked', hasValue);
        rad.change(() => {
          if ( rad.is(':checked') ) {
            individual.set(property_uri, [value]);
          } else {
            individual.removeValue(property_uri, value);
          }
        });
        if (opts.mode === 'view') {
          hld.addClass('disabled');
          rad.attr('disabled', 'disabled');
        }
      }).catch((error) => {
        console.log('Error rendering value', error);
      });
    }));
  }

  /**
   * Individual property modified handler to indicate chosen option
   * @return {void}
   */
  function handler () {
    $('input', self).each((i, el) => {
      const value = $(el).data('value');
      const hasValue = individual.hasValue(property_uri, value);
      $(el).prop('checked', hasValue);
    });
  }

  if (spec && spec.hasValue('v-ui:tooltip')) {
    this.tooltip({
      title: spec['v-ui:tooltip'].map(Util.formatValue).join(' '),
      placement: 'left',
      container: 'body',
      trigger: 'hover',
      animation: false,
    }).one('remove', function (e) {
      $(e.delegateTarget).tooltip('destroy');
    });
  }

  this.on('update', function (e) {
    e.stopPropagation();
    populate();
  });

  this.on('view edit search', function (e) {
    e.stopPropagation();
    if (e.type === 'view') {
      $('div.radio', e.delegateTarget).addClass('disabled');
      $('input', e.delegateTarget).attr('disabled', 'true');
    } else {
      $('div.radio', e.delegateTarget).removeClass('disabled');
      $('input', e.delegateTarget).removeAttr('disabled');
    }
    if (e.type === 'search') {
      const dataDeleted = $(e.delegateTarget).data('deleted');
      withDeleted = typeof dataDeleted === 'boolean' ? dataDeleted : true;
    }
  });
  return this;
};

const defaults = {
  template: `
<div class="radio">
  <label>
    <input type="radio" />
  </label>
</div>
  `,
};
