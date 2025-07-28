// WorkTime control

import $ from 'jquery';

import veda_literal from './veda_literal.js';

import Util from '../../common/util.js';

$.fn.veda_worktime = function ( options ) {
  const opts = {...defaults, ...options};
  const mainInput = veda_literal.call(this, opts);

  this.append( mainInput.hide() );
  this.append( opts.view );

  const pseudoInputs = $('div.input-group>input', this);
  const summaryText = $('#worktime-summary-text', this);
  const isDaily = this.attr('daily') === 'true' || this.attr('daily') === '';
  
  if (isDaily) {
    pseudoInputs.eq(0).closest('div').hide();
  }
  const fillMainInput = function () {
    let count;
    if (isDaily) {
      count = pseudoInputs[1].value*60 + pseudoInputs[2].value*1;
    } else {
      count = pseudoInputs[0].value*480 + pseudoInputs[1].value*60 + pseudoInputs[2].value*1;
    }

    if (count == mainInput.val()) {
      return;
    }
    mainInput.val(count);
    summaryText.text(Util.formatValue(count));
    mainInput.change();
  };
  const fillPseudoInput = function (summaryTime) {
    if (summaryTime && !isDaily) {
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
    } else if (summaryTime && isDaily) {
      summaryText.text(summaryTime);
      summaryTime = parseInt( summaryTime.split(' ').join('').split(',').join('.'), 10 );
      let hours = 0; let minutes = 0;
      if (summaryTime != 0) {
        hours = Math.floor(summaryTime/60);
        summaryTime = summaryTime-hours*60;
        if (summaryTime != 0) {
          minutes = summaryTime;
        }
      }
      pseudoInputs[1].value = hours;
      pseudoInputs[2].value = minutes;
    }
  };
  fillPseudoInput(mainInput.val());
  pseudoInputs.change(fillMainInput);
  mainInput.on('change', e => {
    fillPseudoInput(mainInput.val());
  });
  
  this.on('view edit search', function (e) {
    if (e.type == 'view') {
      pseudoInputs.attr('disabled', 'disabled');
      summaryText.attr('disabled', 'disabled');
    } else {
      pseudoInputs.removeAttr('disabled');
      summaryText.removeAttr('disabled');
    }
    e.stopPropagation();
  });
  return this;
};

const defaults = {
  template: '<input type="text" class="form-control" autocomplete="on" />',
  view: `
<table>
  <tbody>
    <tr>
      <td width="25%">
        <div class="input-group">
          <span class="input-group-addon">DD</span>
          <input type="text" class="form-control">
        </div>
      </td>
      <td width="25%">
        <div class="input-group">
          <span class="input-group-addon">HH</span>
          <input type="text" class="form-control">
        </div>
      </td>
      <td width="25%">
        <div class="input-group">
          <span class="input-group-addon">mm</span>
          <input type="text" class="form-control">
        </div>
      </td>
      <td width="25%">
        <div class="input-group" style="width:100%">
          <span class="input-group-addon">&sum;</span>
          <span id="worktime-summary-text" class="text-right form-control"></span>
        </div>
      </td>
    </tr>
  </tbody>
</table>
  `,
  parser: function (input) {
    const int = parseInt( input.split(' ').join('').split(',').join('.'), 10 );
    return !isNaN(int) ? int : null;
  },
};
