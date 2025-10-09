import $ from 'jquery';

export const post = function (individual, template, container, mode, extra) {
  template = $(template);
  container = $(container);

  const find = container.siblings('.search-actions').find('#search-button.search-button');

  function typeHandler () {
    if (!individual.hasValue('rdf:type')) {
      find.attr('disabled', 'disabled');
      find.addClass('disabled');
      $('veda-control[rel="rdf:type"]', template).addClass('has-error');
    } else {
      find.removeAttr('disabled', 'disabled');
      find.removeClass('disabled');
      $('veda-control[rel="rdf:type"]', template).removeClass('has-error');
    }
  }
  typeHandler();

  individual.on('rdf:type', typeHandler);
  template.one('remove', function () {
    individual.off('rdf:type', typeHandler);
  });
};

export const html = `
  <div>
    <div class="row">
      <div class="col-md-3">
        <em about="rdf:type" property="rdfs:label"></em>
        <veda-control
          rel="rdf:type"
          data-single="true"
          data-type="link"
          class="fulltext dropdown"
          data-query-prefix="'rdfs:subClassOf'==='v-s:UserSearchableDocument'"></veda-control>
      </div>
      <div class="col-md-3">
        <em about="v-fs:SearchForContentBundle" property="rdfs:label"></em>
        <veda-control property="text" data-type="string" placeholder="Введите запрос"></veda-control>
      </div>
      <div class="col-md-3">
        <em about="v-s:created" property="rdfs:label"></em>
        <div property="v-s:created"></div>
        <veda-control data-type="date" property="v-s:created"></veda-control>
      </div>
      <div class="col-md-3">
        <em about="v-s:creator" property="rdfs:label"></em>
        <div property="v-s:creator"></div>
        <veda-control data-type="link" class="fulltext" property="v-s:creator" data-query-prefix="'rdf:type' === 'v-s:Appointment' && 'v-s:official' == 'true'"></veda-control>
      </div>
    </div>
  </div>
`;
