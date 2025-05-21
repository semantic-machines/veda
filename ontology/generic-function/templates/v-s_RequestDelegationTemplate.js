import $ from 'jquery';
import veda from '/js/common/veda.js';
import IndividualModel from '/js/common/individual_model.js';
import Backend from '/js/common/backend.js';

export const pre = function (individual, template, container, mode, extra) {
  template = $(template);
  container = $(container);

  if (individual.hasValue('rdf:type', 'v-s:RequestDelegationAdmin')) {
    $('#delegator-control', template).removeClass('-edit').addClass('edit');
    $('#delegator-value', template).removeClass('edit').addClass('-edit');
    $('#position-control', template).removeClass('-edit').addClass('edit');
    $('#position-container', template).removeClass('-edit').addClass('edit');
  } else {
    $('#custom-label', template).remove();
    $('#ignoreExclusive-control', template).remove();
    $('#official-control', template).remove();
  }
};

export const post = function (individual, template, container, mode, extra) {
  template = $(template);
  container = $(container);

  individual.on('v-s:delegatedPosition', delegatedPositionHandler);
  individual.on('v-s:delegator v-s:delegate', extractEmployee);
  individual.on('v-s:delegator', delegatorHandler);
  template.one('remove', function () {
    individual.off('v-s:delegatedPosition', delegatedPositionHandler);
    individual.off('v-s:delegator', extractEmployee);
    individual.off('v-s:delegate', extractEmployee);
    individual.off('v-s:delegator', delegatorHandler);
  });
  delegatedPositionHandler(individual['v-s:delegatedPosition']);

  const positionTemplate = $('#positions', template).html();
  $('#positions', template)
    .empty()
    .on('click', 'input', function (e) {
      const id = $(this).prop('id');
      const position = new IndividualModel(id);
      if ($(this).is(':checked')) {
        individual['v-s:delegatedPosition'] = individual['v-s:delegatedPosition'].concat(position);
      } else {
        individual['v-s:delegatedPosition'] = individual['v-s:delegatedPosition'].filter(function (item) {
          return item.id !== position.id;
        });
      }
    });

  function delegatedPositionHandler (values) {
    if (values.length) {
      $('#positions', template).removeClass('has-error');
    } else {
      $('#positions', template).addClass('has-error');
    }
  }
  function extractEmployee (property_uri, values) {
    if (values.length && values[0].hasValue('rdf:type', 'v-s:Appointment')) {
      individual[property_uri] = values[0]['v-s:employee'];
    }
  }
  function delegatorHandler (values) {
    $('#positions', template).empty();
    if (values.length) {
      const delegator = values[0];
      const queryStr = "( 'rdf:type' === 'v-s:Appointment' && 'v-s:hasDelegationPurpose'!='d:delegate_Control' && 'v-s:employee' == '" + delegator.id + "' )";
      const delegatedPositions = [];
      Backend.query(veda.ticket, queryStr)
        .then(function (queryResult) {
          return Backend.get_individuals(veda.ticket, queryResult.result);
        })
        .then(function (appointments) {
          const positions_ids = appointments.map(function (appointment_json) {
            const appointment = new IndividualModel(appointment_json);
            if (appointment.hasValue('v-s:dateFrom')) {
              delegatedPositions.push(appointment['v-s:occupation'][0].id);
            }
            return appointment['v-s:occupation'][0].id;
          });
          return positions_ids;
        })
        .then(function (positions_ids) {
          return Backend.get_individuals(veda.ticket, positions_ids);
        })
        .then(function (positions) {
          positions.map(function (position_json) {
            const position = new IndividualModel(position_json);
            const tmpl = $(positionTemplate);
            const input = tmpl.find('.input');
            const label = tmpl.find('.position-label');
            if (individual.hasValue('v-s:delegatedPosition', position)) {
              input.prop('checked', 'checked');
            }

            input.prop('id', position.id);

            if (delegatedPositions.indexOf(position.id) >= 0) {
              label.text(position.toString() + ' (делегировано)');
            } else {
              label.text(position.toString());
            }
            if (mode === 'view') {
              input.prop('disabled', 'disabled');
            }

            tmpl.appendTo($('#positions', template));
          });
        });
    }
  }

  if (individual.isNew() && !individual.hasValue('v-s:delegator') && template.attr('data-mode') != 'search') {
    individual['v-s:delegator'] = [veda.user];
  } else if (individual.isNew() && individual.hasValue('v-s:delegator')) {
    delegatorHandler(individual['v-s:delegator']);
  }

  template.on('view edit', function (e) {
    if (e.type === 'view') {
      $('#positions', template).find('.input').prop('disabled', 'disabled');
    } else {
      $('#positions', template).find('.input').prop('disabled', false);
    }
  });
};

export const html = `
  <div class="container sheet">
    <h2 about="@" rel="rdf:type" data-template="v-ui:LabelTemplate"></h2>
    <hr />
    <div class="row">
      <div class="col-md-6 col-xs-12">
        <em about="v-s:hasDelegationPurpose" property="rdfs:label"></em>
        <div rel="v-s:hasDelegationPurpose" data-template="v-ui:LabelTemplate" class="view -edit search"></div>
        <veda-control data-type="link" rel="v-s:hasDelegationPurpose" class="-view edit search fulltext dropdown" data-deleted="true"></veda-control>
      </div>
    </div>
    <div class="row" id="official-control">
      <div class="col-md-6 col-xs-12">
        <div class="checkbox">
          <label>
            <veda-control property="v-s:official" data-type="boolean"></veda-control>
            <em about="v-s:official" property="rdfs:label"></em>
          </label>
        </div>
      </div>
    </div>
    <div class="row" id="ignoreExclusive-control">
      <div class="col-md-6 col-xs-12">
        <div class="checkbox">
          <label>
            <veda-control property="v-s:ignoreExclusive" data-type="boolean"></veda-control>
            <em about="v-s:ignoreExclusive" property="rdfs:label"></em>
          </label>
        </div>
      </div>
    </div>
    <div class="row">
      <div class="col-md-6 col-xs-12">
        <em about="v-s:delegator" property="rdfs:label"></em>
        <div about="@" id="delegator-value" rel="v-s:delegator" data-template="v-ui:LabelTemplate" class="view edit -search"></div>
        <div rel="v-s:delegator" data-template="v-ui:LabelTemplate" class="-view -edit search"></div>
        <veda-control id="delegator-control" data-type="link" rel="v-s:delegator" class="-view -edit search fulltext" data-deleted="true"></veda-control>
      </div>
    </div>
    <div class="row">
      <div class="col-md-6 col-xs-12">
        <em about="v-s:delegatedPosition" property="rdfs:label"></em>
        <div id="position-container" rel="v-s:delegatedPosition" data-template="v-ui:LabelTemplate" class="view -edit search"></div>
        <veda-control id="position-control" data-type="link" rel="v-s:delegatedPosition" class="-view -edit search fulltext" data-deleted="true"></veda-control>
        <div id="positions" class="-view edit -search">
          <div class="checkbox">
            <label>
              <input class="input" type="checkbox" value="" />
              <span class="position-label"></span>
            </label>
          </div>
        </div>
      </div>
    </div>
    <div class="row">
      <div class="col-md-6 col-xs-12">
        <em about="v-s:delegate" property="rdfs:label"></em>
        <div rel="v-s:delegate" data-template="v-ui:LabelTemplate" class="view -edit search"></div>
        <veda-control data-type="link" rel="v-s:delegate" class="-view edit search fulltext"></veda-control>
      </div>
    </div>
    <div class="row" id="custom-label">
      <div class="col-md-6 col-xs-12">
        <em about="rdfs:label" property="rdfs:label"></em>
        <div property="rdfs:label" class="view -edit -search"></div>
        <veda-control data-type="multilingualString" property="rdfs:label" class="-view edit search"></veda-control>
      </div>
    </div>
    <div class="row">
      <div class="col-md-3 col-xs-6">
        <em about="v-s:dateFrom" property="rdfs:label"></em>
        <div property="v-s:dateFrom" class="view -edit search"></div>
        <veda-control property="v-s:dateFrom" data-type="date" class="-view edit search"></veda-control>
      </div>
      <div class="col-md-3 col-xs-6">
        <em about="v-s:dateTo" property="rdfs:label"></em>
        <div property="v-s:dateTo" class="view -edit search"></div>
        <veda-control property="v-s:dateTo" data-type="date" class="-view edit search"></veda-control>
      </div>
    </div>
    <br />
    <section class="view -edit -search">
      <h4 class="section-header" about="v-s:createdAppointment" property="rdfs:label"></h4>
      <div class="table-responsive">
        <table class="table">
          <thead>
            <tr>
              <th width="1%"><span class="glyphicon glyphicon-search"></span></th>
              <th about="v-s:employee" property="rdfs:label"></th>
              <th about="v-s:occupation" property="rdfs:label"></th>
              <th about="v-s:dateFrom" property="rdfs:label"></th>
              <th about="v-s:dateTo" property="rdfs:label"></th>
              <th about="v-s:origin" property="rdfs:label"></th>
              <th about="v-s:created" property="rdfs:label"></th>
            </tr>
          </thead>
          <tbody rel="v-s:createdAppointment">
            <tr>
              <td width="1%"><a href="#/@" class="glyphicon glyphicon-search"></a></td>
              <td rel="v-s:employee" data-template="v-ui:LabelTemplate"></td>
              <td rel="v-s:occupation" data-template="v-ui:LabelTemplate"></td>
              <td property="v-s:dateFrom"></td>
              <td property="v-s:dateTo"></td>
              <td property="v-s:origin"></td>
              <td property="v-s:created"></td>
            </tr>
          </tbody>
        </table>
      </div>
    </section>
    <br />
    <div class="actions view edit -search">
      <span about="@" data-template="v-ui:StandardButtonsTemplate" data-embedded="true" data-buttons="edit save cancel delete"></span>
    </div>
  </div>
`;
