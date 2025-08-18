import $ from 'jquery';

export const pre = function (individual, template, container, mode, extra) {
  template = $(template);
  container = $(container);

  // Добавляем обработчик валидации
  template.on('validate', function () {
    const result = {};
    
    // Проверка v-s:date - всегда обязательно
    if (!individual.hasValue('v-s:date')) {
      result['v-s:date'] = {
        state: false,
        cause: ['v-ui:minCardinality'],
      };
    }
    
    // Отправляем результат валидации
    template[0].dispatchEvent(new CustomEvent('validated', {detail: result}));
  });
};

export const post = function (individual, template, container, mode, extra) {
  template = $(template);
  container = $(container);
  
  // Удаляем слушатели при удалении шаблона
  template.one('remove', function() {
    // Нет необходимости в слушателях
  });
  
  // Инициализация пользовательских шаблонов для отображения валют без крестика
  template.find('.currency-readonly').each(function() {
    const $this = $(this);
    const rel = $this.attr('rel');
    if (individual.hasValue(rel)) {
      const currency = individual[rel][0];
      currency.load().then(function(loaded) {
        if (loaded && loaded.hasValue('rdfs:label')) {
          $this.text(loaded['rdfs:label'][0]);
        } else {
          $this.text(loaded.id);
        }
      });
    }
  });
};

export const html = `
  <div class="container sheet">
    <h3 class="margin-sm">
      <span about="v-s:CurrencyExchangeRate" property="rdfs:label"></span>
      <small about="@" property="rdfs:label"></small>
    </h3>
    <span about="@" data-template="v-ui:RabbitHole"></span>
    <hr />
    <div class="row row-attribute">
      <div class="col-sm-3 col-xs-5"></div>
      <div class="col-sm-9 col-xs-7">
        <div class="checkbox no-margin">
          <label>
            <veda-control property="v-s:valid" data-type="boolean"></veda-control>
            <span about="v-s:valid" property="rdfs:label"></span>
          </label>
        </div>
      </div>
    </div>
    <div class="row row-attribute">
      <div class="col-sm-3 col-xs-5">
        <label about="v-s:date" property="rdfs:label"></label>
      </div>
      <div class="col-sm-3 col-xs-3">
        <div property="v-s:date" class="view -edit search"></div>
        <veda-control data-type="date" property="v-s:date" class="-view edit search"></veda-control>
      </div>
    </div>
    <div class="row row-attribute">
      <div class="col-sm-3 col-xs-5">
        <label about="v-s:hasCurrencyExchangeRatePurpose" property="rdfs:label"></label>
      </div>
      <div class="col-sm-9 col-xs-7">
        <div class="currency-readonly view edit -search" rel="v-s:hasCurrencyExchangeRatePurpose"></div>
        <div rel="v-s:hasCurrencyExchangeRatePurpose" data-template="v-ui:LabelTemplate" class="-view -edit -search"></div>
        <veda-control data-type="link" rel="v-s:hasCurrencyExchangeRatePurpose" class="-view -edit search fulltext dropdown"></veda-control>
      </div>
    </div>
    <div class="row row-attribute">
      <div class="col-sm-3 col-xs-5"></div>
      <div class="col-sm-9 col-xs-7">
        <table class="table table-striped table-bordered">
          <tr>
            <th width="5%"></th>
            <th width="30%" about="v-s:hasCurrency" property="rdfs:label"></th>
            <th width="5%"></th>
            <th width="25%" about="v-s:rate" property="rdfs:label"></th>
            <th width="30%" about="v-s:hasCurrency" property="rdfs:label"></th>
          </tr>
          <tr>
            <td>
              <div class="col-md-4">
                <strong>1</strong>
              </div>
            </td>
            <td>
              <div class="currency-readonly view edit -search" rel="v-s:hasCurrencySource"></div>
              <div rel="v-s:hasCurrencySource" data-template="v-ui:LabelTemplate" class="-view -edit -search"></div>
              <veda-control data-type="link" rel="v-s:hasCurrencySource" class="-view -edit search fulltext dropdown"></veda-control>
            </td>
            <td>
              <div class="col-md-4">
                <strong>=</strong>
              </div>
            </td>
            <td>
              <div property="v-s:rate" class="view -edit search"></div>
              <veda-control data-type="decimal" property="v-s:rate" class="-view edit search"></veda-control>
            </td>
            <td>
              <div class="currency-readonly view edit -search" rel="v-s:hasCurrencyTarget"></div>
              <div rel="v-s:hasCurrencyTarget" data-template="v-ui:LabelTemplate" class="-view -edit -search"></div>
              <veda-control data-type="link" rel="v-s:hasCurrencyTarget" class="-view -edit search fulltext dropdown"></veda-control>
            </td>
          </tr>
        </table>
      </div>
    </div>
    <div class="row row-attribute">
      <div class="col-sm-3 col-xs-5">
        <label about="v-s:Comment" property="rdfs:label"></label>
      </div>
      <div class="col-sm-9 col-xs-7">
        <div property="v-s:Comment" class="view -edit -search"></div>
        <veda-control data-type="string" property="v-s:Comment" class="-view edit search"></veda-control>
      </div>
    </div>
    <hr />
    <!-- BUTTONS -->
    <div class="actions view edit -search">
      <span about="@" data-template="v-ui:StandardButtonsTemplate" data-embedded="true" data-buttons="edit save cancel"></span>
    </div>
  </div>
`;
