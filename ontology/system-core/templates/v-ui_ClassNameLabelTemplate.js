import $ from 'jquery';

export const post = async function (individual, template, container, mode, extra) {
  template = $(template);
  container = $(container);

  let showIcon = false;
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
  if (!showIcon) {
    $('.glyphicon', template).remove();
  }

  if (!individual.hasValue('rdfs:label')) {
    $('#label', template).text(individual.id);
  }
};

export const html = `
  <span>
    <span class="glyphicon glyphicon-exclamation-sign view edit -search"></span>
    <span about="@" rel="rdf:type">
      <span about="@" property="rdfs:label"></span>
    </span>:
    <span id="label" about="@" property="rdfs:label"></span>
  </span>
`;