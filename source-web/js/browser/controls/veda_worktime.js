// WorkTime control

import $ from 'jquery';

import veda_literal from './veda_literal.js';

import Util from '../../common/util.js';

$.fn.veda_worktime = function ( options ) {
  const opts = $.extend( {}, $.fn.veda_worktime.defaults, options );
  const mainInput = veda_literal.call(this, opts);

  this.append( mainInput.hide() );
  this.append( $('#worktime-control-template').html() );

  const pseudoInputs = $('div.input-group>input', this);
  const summaryText = $('#worktime-summary-text', this);
  const fillMainInput = function () {
    const count = pseudoInputs[0].value*480 + pseudoInputs[1].value*60 + pseudoInputs[2].value*1;
    mainInput.val(count);
    summaryText.text(Util.formatValue(count));
    mainInput.change();
  };
  const fillPseudoInput = function (summaryTime) {
    if (summaryTime) {
      summaryText.text(summaryTime);
      summaryTime = parseInt( summaryTime.split(' ').join('').split(',').join('.'), 10 );
      let days = 0; let hours = 0; let minutes = 0;
      if (summaryTime != 0) {
        days = Math.floor(summaryTime/480);
        summaryTime = summaryTime-days*480;
        if (summaryTime != 0) {
          hours = Math.floor(summaryTime/60);
          summaryTime = summaryTime-hours*60;
          if (summaryTime != 0) {
            minutes = summaryTime;
          }
        }
      }
      pseudoInputs[0].value = days;
      pseudoInputs[1].value = hours;
      pseudoInputs[2].value = minutes;
    }
  };
  fillPseudoInput(mainInput.val());
  pseudoInputs.change(fillMainInput);
  this.on('view edit search', function (e) {
    e.stopPropagation();
  });
  return this;
};
$.fn.veda_worktime.defaults = {
  parser: function (input) {
    const int = parseInt( input.split(' ').join('').split(',').join('.'), 10 );
    return !isNaN(int) ? int : null;
  },
};