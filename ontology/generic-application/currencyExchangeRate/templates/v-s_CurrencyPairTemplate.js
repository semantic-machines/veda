import BrowserUtil from '/js/browser/util.js';
import $ from 'jquery';
import veda from '/js/common/veda.js';
import IndividualModel from '/js/common/individual_model.js';
import riot from 'riot';

export const pre = async function (individual, template, container, mode, extra) {
  template = $(template);
  container = $(container);

}

export const post = function (individual, template, container, mode, extra) {
  template = $(template);
  container = $(container);

  $('#add-CurrencyExchangeRate', template).click(function (e) {
    e.stopPropagation();
    const _class = new IndividualModel('v-s:CurrencyExchangeRate');
    const CurrencyExchangeRate = new IndividualModel();
    const tmpl = 'v-s:CurrencyExchangeRateTemplate';
    CurrencyExchangeRate['rdf:type'] = [_class];
    CurrencyExchangeRate['v-s:backwardTarget'] = [individual];
    CurrencyExchangeRate['v-s:backwardProperty'] = [new IndividualModel('v-s:hasCurrencyExchangeRate')];
    CurrencyExchangeRate['v-s:hasCurrencySource'] = individual['v-s:hasCurrencySource'];
    CurrencyExchangeRate['v-s:hasCurrencyTarget'] = individual['v-s:hasCurrencyTarget'];
    CurrencyExchangeRate['v-s:hasCurrencyExchangeRatePurpose'] = individual['v-s:hasCurrencyPairPurpose'];
    CurrencyExchangeRate['v-s:hasVisualStatus'] = [new IndividualModel('v-s:StatusActive')];
    individual['v-s:hasCurrencyExchangeRate'] = individual['v-s:hasCurrencyExchangeRate'].concat([CurrencyExchangeRate]);
    
    BrowserUtil.showModal(CurrencyExchangeRate, tmpl, 'edit');
  });

  
};
export const html = `
<div class="container sheet">
<h3 class="margin-sm">
  <span about="v-s:CurrencyPair" property="rdfs:label"></span>
  <small about="@" property="rdfs:label"></small>
</h3>
<hr />
<div class="row row-attribute">
  <div class="col-sm-3 col-xs-5">
    <label about="v-s:hasStatus" property="rdfs:label"></label>
  </div>
  <div class="col-sm-9 col-xs-7">
    <div rel="v-s:hasVisualStatus" data-template="v-ui:StatusTemplate" class="view -edit search"></div>
    <veda-control data-type="link" rel="v-s:hasVisualStatus" class="-view edit search fulltext dropdown" style="width: 50%"></veda-control>
  </div>
</div>
<div class="row row-attribute">
  <div class="col-sm-3 col-xs-5">
    <label about="v-s:hasCurrencyPairPurpose" property="rdfs:label"></label>
  </div>
  <div class="col-sm-9 col-xs-7">
    <div rel="v-s:hasCurrencyPairPurpose" data-template="v-ui:LabelTemplate" class="view -edit search"></div>
    <veda-control data-type="link" rel="v-s:hasCurrencyPairPurpose" class="-view edit search fulltext dropdown" style="width: 50%"  ></veda-control>
  </div>
</div>
<div class="row row-attribute">
  <div class="col-sm-3 col-xs-5">
    <label about="v-s:hasCurrencySource" property="rdfs:label"></label>
  </div>
  <div class="col-sm-9 col-xs-7">
    <div rel="v-s:hasCurrencySource" data-template="v-ui:LabelTemplate" class="view -edit search"></div>
    <veda-control data-type="link" id="hasCurrencySource" rel="v-s:hasCurrencySource" class="-view edit search fulltext dropdown" style="width: 50%"></veda-control>
  </div>
</div>
<div class="row row-attribute">
  <div class="col-sm-3 col-xs-5">
    <label about="v-s:hasCurrencyTarget" property="rdfs:label"></label>
  </div>
  <div class="col-sm-9 col-xs-7">
    <div rel="v-s:hasCurrencyTarget" data-template="v-ui:LabelTemplate" class="view -edit search"></div>
    <veda-control data-type="link" id="hasCurrencyTarget" rel="v-s:hasCurrencyTarget" class="-view edit search fulltext dropdown" style="width: 50%"></veda-control>
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
<section id="VersionsDocuments" class="view edit -search">
      <h4 class="section-header">
        <span about="mnd-s:CurrencyExchangeRate" property="rdfs:label"></span>
        <button class="btn btn-xs btn-success pull-right margin-lg-h view -edit -search" id="add-CurrencyExchangeRate">
          <span class="glyphicon glyphicon-zoom-in"></span>
          <span about="mnd-s:CurrencyExchangeRate" property="rdfs:label"> </span>
        </button>
        <span about="v-s:hasCurrencyExchangeRate" data-template="v-ui:SectionHeaderTemplate"></span>
      </h4>
      <div class="section-content">
        <table class="table table-condensed table-bordered">
          <thead>
            <tr class="view edit -search active">
              <th width="1%"><span class="glyphicon glyphicon-search"></th>
              <th about="v-s:date" property="rdfs:label"></th>
              <th width="5%"></th>
              <th about="v-s:hasCurrencySource" property="rdfs:label"></th>
              <th width="5%"></th>
              <th about="v-s:rate" property="rdfs:label"></th>
              <th about="v-s:hasCurrencyTarget" property="rdfs:label"></th>
              <th about="v-s:valid" property="rdfs:label"></th>
            </tr>
          </thead>
          <tbody rel="v-s:hasCurrencyExchangeRate">
            <tr>
              <td about="@" data-template="v-ui:IconModalTemplate"></td>
              <td property="v-s:date" data-template="v-ui:DateTemplate"></td>
              <td>1</td>
              <td property="v-s:hasCurrencySource" data-template="v-ui:LabelTemplate"></td>
              <td>=</td>
              <td property="v-s:rate" data-template="v-ui:LabelTemplate"></th>
              <td property="v-s:hasCurrencyTarget" data-template="v-ui:LabelTemplate"></td>
              <td property="v-s:valid" data-template="v-ui:LabelTemplate"></th>
            </tr>
          </tbody>
        </table>
      </div>
    </section>
<!-- BUTTONS -->
<div class="actions view edit -search">
  <span about="@" data-template="v-ui:StandardButtonsTemplate" data-embedded="true" data-buttons="edit save cancel"></span>
</div>
</div>
`;
