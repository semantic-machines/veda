import $ from 'jquery';

export const pre = function (individual, template, container, mode, extra) {
  template = $(template);
  container = $(container);
  console.log('appointment popover');
  if (individual.id.indexOf('cfg:') < 0) {
    $('.open-structure', template).click(function() {
      window.open('/#/v-s:Contacts////target='+individual.id);
    });
  } else {
    $('.open-structure', template).remove();
  }

  if (individual.hasValue('v-s:employee')) {
    return individual['v-s:employee'][0].load().then(function (employee) {
      if (!employee.hasValue('v-s:hasImage')) {
        $('.media-left', template).remove();
      }
      if (!employee.hasValue('v-s:dateAbsenceTo')) {
        $('.absence-block', template).remove();
      }
      if (!employee.hasValue('v-s:delegate')) {
        $('.delegate-block', template).remove(); 
      }
      if (employee.hasValue('v-s:hasVisualStatus','v-s:StatusWorking')) {
        $('.status-block', template).remove();
      }
    });
  }
};
export const post = function (individual, template, container, mode, extra) {
  template = $(template);
  container = $(container);

  if (individual.hasValue('v-s:employee')) {
    if (individual.hasValue('v-s:deleted',true)) {
      console.log('deleted');
      template.removeClass('deleted');
    }
  }
};

export const html = `
  <div class="media" style="margin-top:0px;">
    <span class="close">&nbsp;&times;</span>
    <div class="media-left" about="@" rel="v-s:employee" style="width:96px">
        <a href="#/@" about="@" rel="v-s:hasImage" data-template="v-ui:ImageTemplate"></a>
    </div>
    <div class="media-body" style="width:auto;">
      <div class="status-block" about="@" rel="v-s:employee">
        <div about="@" rel="v-s:hasVisualStatus"></div>
      </div>
      <strong class="media-heading" about="@" rel="v-s:employee">
        <span>
          <span about="@" property="v-s:lastName"></span>
          <span about="@" property="v-s:firstName"></span>
          <span about="@" property="v-s:middleName"></span>
        </span>
      </strong>
      <hr class="no-margin" />
      <div about="@" rel="v-s:employee">
          <div about="@" rel="v-s:defaultAppointment">
            <div about="@" rel="v-s:occupation">
              <div>
                <small about="@" property="v-s:title"></small>
                <br />
                <small about="@" rel="v-s:parentUnit" data-template="v-ui:LabelTemplate"></small>
              </div>
            </div>
          </div>
      </div>
      <div about="@" rel="v-s:employee">
        <div>
          <div about="@" rel="v-s:hasCommunicationMean">
            <div>
              <small about="@" property="v-s:description"></small>
            </div>
          </div>
          <hr class="no-margin" />
          <div class="absence-block">
            <div>
              <small>
                <span about="v-s:AbsenceUntilBundle" property="rdfs:label"></span>
                <span about="@" property="v-s:dateAbsenceTo"></span>
              </small>
            </div>
          </div>
          <div class="delegate-block">
            <div>
              <small>
                <span about="v-s:delegate" property="rdfs:label"></span>
                <span about="@" rel="v-s:delegate" data-template="v-ui:LabelTemplate"></span>
              </small>
            </div>
          </div>
        </div>
      </div>
      <small class="open-structure pointer">
        <a about="v-s:ShowInContactsBundle" property="rdfs:label"></a>
      </small>
    </div>
  </div>
`;
