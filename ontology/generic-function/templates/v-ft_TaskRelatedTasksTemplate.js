import $ from 'jquery';
import IndividualModel from '/js/common/individual_model.js';

export const pre = async function (individual, template, container, mode, extra) {
  template = $(template);
  container = $(container);

  const relatedTasks = new IndividualModel('v-ft:RelatedTasks');
  try {
    await relatedTasks.load();
    relatedTasks.clearValue('v-fs:searchResult');
    const taskId = individual.id;
    const docId = individual.hasValue('v-wf:onDocument') && individual['v-wf:onDocument'][0].id;
    const personId = veda.user.id;
    const positionId = veda.appointment && veda.appointment.hasValue('v-s:occupation') ? veda.appointment['v-s:occupation'][0].id : undefined;

    // Найти все назначения пользователя
    const delegatePositions = [];
    const queryAppointments = "'rdf:type'=='v-s:Appointment' && 'v-s:employee'=='" + personId + "'";
    const response = await veda.Backend.query(queryAppointments);
    if (response.result.length > 0) {
      for (const appointmentUri of response.result) {
        const appointment = new veda.IndividualModel(appointmentUri);
        await appointment.load();      
        if (appointment && appointment.hasValue('v-s:occupation')) {
          delegatePositions.push(appointment['v-s:occupation'][0].id);
        }
      }
    }

    // Сформировать строку для поиска по должностям
    const delegatePositionsQuery = delegatePositions.map(function(positionId) {
      return "'v-wf:to'=='" + positionId + "' || 'v-wf:from'=='" + positionId + "'";
    }).join(' || ');

    const queryStr = [
      "'@' != '" + taskId + "'",
      "'v-wf:onDocument' == '" + docId + "'",
      "'rdf:type'==='v-wf:DecisionForm'", 
      "'v-wf:isCompleted'== false",
      "('v-wf:to'=='" + personId + "' || 'v-wf:to'=='" + positionId + "' || " + delegatePositionsQuery + " || 'v-wf:from'=='" + personId + "' || 'v-wf:from'=='" + positionId + "')",
    ].join(' && ');

    // Проверяем есть ли результаты по запросу
    const checkResults = await veda.Backend.query(queryStr);
    if (checkResults.result.length > 0) {
      relatedTasks['v-fs:fulltextQuery'] = [queryStr];
      template.html(html);
    } else {
      template.html('');
    }
  } catch (error) {
    console.error('Error in pre function:', error);
  }
};

export const html = `
  <div>
    <div about="v-ft:RelatedTasks" data-template="v-fs:AttributiveSearchInlineTemplate"></div>
  </div>
`;