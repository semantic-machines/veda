import $ from 'jquery';

export const post = async function (individual, template, container, mode, extra) {
  template = $(template);
  container = $(container);

  if (!individual.hasValue('rdfs:label')) {
    template.text(individual.id);
  }
  let done;
  let initPopoverTimeout;
  let showIcon = false;
  const type = individual.hasValue('rdf:type') && individual['rdf:type'][0].id;
  if (type === 'v-s:Appointment' || type === 'v-s:Person' || type === 'v-s:Position' || type === 'v-s:Organization') {
    template
      .css('cursor', 'help')
      .click(function () {
        if (done) return;
        initPopover();
      })
      .one('remove', function () {
        template.popover('destroy');
      });
    if (individual.hasValue('v-s:hasVisualStatus')) {
      const status = await individual['v-s:hasVisualStatus'][0].load();
      if (status.hasValue('v-s:tag','danger')) {
        showIcon = true;
        $('.glyphicon', template).attr('style', 'color: #d9534f');
      } else if (status.hasValue('v-s:tag','warning')) {
        showIcon = true;
        $('.glyphicon', template).attr('style', 'color: #f0ad4e');
      }
    }
    if (type === 'v-s:Appointment') {
      const person = await individual['v-s:employee'][0].load();
      if (person.hasValue('v-s:hasVisualStatus')) {
        const status = await person['v-s:hasVisualStatus'][0].load();
        if (status.hasValue('v-s:tag','danger')) {
          showIcon = true;
          $('.glyphicon', template).attr('style', 'color: #d9534f');
        } else if (status.hasValue('v-s:tag','warning') || individual.hasValue('v-s:deleted',true)) {
          showIcon = true;
          $('.glyphicon', template).attr('style', 'color: #f0ad4e');
        }
      } else if (individual.hasValue('v-s:deleted',true)) {
        showIcon = true;
        if (person.hasValue('v-s:deleted',true)) {
          $('.glyphicon', template).attr('style', 'color: #d9534f');
        } else {
          $('.glyphicon', template).attr('style', 'color: #f0ad4e');
        }
      }
    }
  }

  if (!showIcon) {
    $('.glyphicon', template).remove();
  }

  function initPopover () {
    const cntr = $('<div></div>');
    let tmpl;
    if (type === 'v-s:Appointment') {
      tmpl = 'v-ui:AppointmentPopoverTemplate';
    } else if (type === 'v-s:Person') {
      tmpl = 'v-ui:PersonPopoverTemplate';
    } else if (type === 'v-s:Organization') {
      tmpl = 'v-ui:OrganizationPopoverTemplate';
    } else {
      tmpl = 'v-ui:PositionPopoverTemplate';
    }
    individual.present(cntr, tmpl).then(function () {
      done = true;
      template
        .popover({
          trigger: 'click',
          placement: 'auto bottom',
          html: true,
          container: $('div#main'),
          content: cntr,
        })
        .click();
      cntr.on('click', '.close', function (e) {
        e.stopPropagation();
        template.click();
      });
    });
  };
};

export const html = `
<span class="label-template">
  <span class="glyphicon glyphicon-exclamation-sign view edit -search"></span>
  <span about="@" property="rdfs:label"></span>
</span>
`;
