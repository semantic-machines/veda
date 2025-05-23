// Date control

import $ from 'jquery';

import moment from 'moment';

import veda_dateTime from './veda_datetime.js';

$.fn.veda_date = function ( options ) {
  const opts = {...defaults, ...options};
  const control = veda_dateTime.call(this, opts);

  const tabindex = this.attr('tabindex');
  if (tabindex) {
    this.removeAttr('tabindex');
    control.find('input').attr('tabindex', tabindex);
  }

  this.append(control);
  return this;
};

const defaults = {
  template: `
  <div class="date-control">
    <div class="input-group">
      <span class="input-group-addon btn btn-default date">
        <span class="glyphicon glyphicon-time"></span>
      </span>
      <input type="text" class="form-control" autocomplete="off"/>
      <div class="input-group-addon btn btn-default clear" tabindex="0">&#10005;</div>
      <ul class="add-time-list dropdown-menu dropdown-menu-right"></ul>
      <div class="input-group-addon btn btn-default dropdown-toggle" tabindex="0" data-toggle="dropdown" aria-haspopup="true" aria-expanded="false">
        <i class="caret"></i>
      </div>
    </div>
  </div>
  `,
  parser: function (input) {
    if (input) {
      const timestamp = moment(input, 'DD.MM.YYYY').toDate();
      const symbolicDate = new Date(timestamp);
      const d = symbolicDate.getDate();
      const m = symbolicDate.getMonth();
      const y = symbolicDate.getFullYear();
      symbolicDate.setUTCFullYear(y, m, d);
      symbolicDate.setUTCHours(0, 0, 0, 0);
      return symbolicDate;
    }
    return null;
  },
  format: 'DD.MM.YYYY',
};
