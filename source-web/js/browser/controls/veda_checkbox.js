// Checkbox control

import $ from 'jquery';

import Util from '../../common/util.js';

import IndividualModel from '../../common/individual_model.js';

import {interpolate, ftQuery, ftQueryWithDeleted, renderValue} from './veda_control_util.js';

$.fn.veda_checkbox = function (params) {
  const opts = {...defaults, ...params};
  const self = this;
  const individual = opts.individual;
  const property_uri = opts.property_uri || opts.rel_uri;
  const spec = opts.spec;
  const rangeRestriction = spec?.hasValue('v-ui:rangeRestriction') ? spec['v-ui:rangeRestriction'][0] : undefined;
  const range = rangeRestriction ? [rangeRestriction] : (new IndividualModel(property_uri))['rdfs:range'];
  const queryPattern = this.attr('data-query-pattern') ?? (spec?.hasValue('v-ui:queryPattern') ? spec['v-ui:queryPattern'][0].toString() : undefined);
  const queryPrefixDefault = this.attr('data-query-prefix') || ( spec?.hasValue('v-ui:queryPrefix') ? spec['v-ui:queryPrefix'][0] : range.map((item) => {
    return '\'rdf:type\'===\'' + item.id + '\'';
  }).join(' || ') );
  const isDynamicQueryPrefix = this.attr('data-dynamic-query-prefix') == 'true';
  const sort = this.attr('data-sort') || ( spec?.hasValue('v-ui:sort') ? spec['v-ui:sort'][0].toString() : undefined );
  const source = this.attr('data-source') || undefined;
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
   * Handle checkbox change event
   * @param {jQuery} chk
   * @param {*} value
   */
  function handleCheckboxChange(chk, value) {
    if (chk.is(':checked')) {
      individual.addValue(property_uri, value);
    } else {
      individual.removeValue(property_uri, value);
    }
  }

  /**
   * Populate options list
   * @return {Promise}
   */
  function populate () {
    if (spec?.hasValue('v-ui:optionValue')) {
      const options = spec['v-ui:optionValue'];
      return renderOptions(options);
    } else if (source) {
      return Promise.resolve(eval(source))
        .then(renderOptions)
        .catch((error) => {
          console.error('Source failed', source);
        });
    } else if (queryPrefixDefault) {
      let queryPrefix;
      if (isDynamicQueryPrefix) {
        queryPrefix = self.attr('data-query-prefix');
      }
      if (queryPrefix == undefined) {
        queryPrefix = queryPrefixDefault;
      }
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
   * Setup checkbox element
   * @param {*} rendered
   * @param {jQuery} hld
   * @param {*} value
   */
  function setupCheckbox(rendered, hld, value) {
    const lbl = $('label', hld).append(rendered);
    const chk = $('input', lbl).data('value', value);
    if (value instanceof IndividualModel && value.hasValue('v-s:deleted', true)) {
      hld.addClass('deleted');
    }
    const hasValue = individual.hasValue(property_uri, value);
    chk.prop('checked', hasValue);
    chk.change(() => handleCheckboxChange(chk, value));
    if (opts.mode === 'view') {
      hld.addClass('disabled');
      chk.attr('disabled', 'disabled');
    }
  }

  /**
   * Process single option
   * @param {*} value
   * @param {number} index
   * @return {Promise}
   */
  function processOption(value, index) {
    if (index >= 100) {
      return Promise.resolve();
    }
    const hld = $(opts.template).appendTo(self);
    return renderValue(value, template)
      .then((rendered) => setupCheckbox(rendered, hld, value))
      .catch((error) => {
        console.log('Error rendering value', error);
      });
  }

  /**
   * Render options list
   * @param {Array} options
   * @return {Promise}
   */
  function renderOptions (options) {
    self.empty();
    const optionsPromises = options.map(processOption);
    return Promise.all(optionsPromises);
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

  if (spec?.hasValue('v-ui:tooltip')) {
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
      $(e.delegateTarget).children().addClass('disabled');
      $('input', e.delegateTarget).attr('disabled', 'true');
    } else {
      $(e.delegateTarget).children().removeClass('disabled');
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
<div class="checkbox">
  <label>
    <input type="checkbox" />
  </label>
</div>
  `,
};
