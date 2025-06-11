import CommonUtil from '/js/common/util.js';
import BrowserUtil from '/js/browser/util.js';
import $ from 'jquery';
import veda from '/js/common/veda.js';
import IndividualModel from '/js/common/individual_model.js';
import Backend from '/js/common/backend.js';
import riot from 'riot';
import notify from '/js/browser/notify.js';

export const pre = function (individual, template, container, mode, extra) {
  template = $(template);
  container = $(container);

  $('#refresh', template).on('click', refresh);

  function refresh () {
    template
      .parent()
      .closest('[resource]')
      .find('[resource]')
      .addBack('[resource]')
      .each(function () {
        const uri = $(this).attr('resource');
        const resource = new IndividualModel(uri);
        resource.is('v-s:UserThing').then(function (isUserThing) {
          if (isUserThing) {
            resource.reset();
          }
        });
      });
  }

  let toRefresh;

  function statusHandler (status) {
    if (status === 'online' || status === 'offline') {
      toRefresh = false;
      $('#refresh', template).addClass('hidden');
    } else if (status === 'limited') {
      toRefresh = true;
      $('#refresh', template).removeClass('hidden');
    }
  }
  veda.on('status', statusHandler);
  template.one('remove', function () {
    veda.off('status', statusHandler);
  });

  template.on('click', '#edit, #save, #cancel, #delete, #recover, #destroy', function (e) {
    e.preventDefault();
    const action = $(this).attr('data-action');
    if (action === 'destroy') {
      const warning = new IndividualModel('v-s:AreYouSure');
      warning.load().then(function (warning) {
        if (confirm(warning['rdfs:label'].map(CommonUtil.formatValue).join(' '))) {
          template.parent().closest('[resource]')[0].dispatchEvent(new Event(action));
        }
      });
    } else if (action === 'delete') {
      const queryString = "'rdf:type'==='v-wf:DecisionForm' && 'v-wf:onDocument'=='" + individual.id + "' && 'v-wf:isCompleted'==false";
      Backend.query(veda.ticket, queryString).then(function (queryResult) {
        const tmp = queryResult.result;
        if (tmp.length == 0) {
          const warning = new IndividualModel('v-s:AreYouSure');
          warning.load().then(function (warning) {
            if (confirm(warning['rdfs:label'].map(CommonUtil.formatValue).join(' '))) {
              template.parent().closest('[resource]')[0].dispatchEvent(new Event(action));
            }
          });
        } else {
          alert('Документ не может быть удален, так как по нему есть незакрытые задачи. Закройте все задачи и попробуйте ещё раз');
        }
      });
    } else {
      template.parent().closest('[resource]')[0].dispatchEvent(new Event(action));
    }
  });
  // $('#cancel', template).on('click', function () {
  //   template.closest('.modal').modal('hide').remove();
  // });
  $('#journal', template).on('click', function (e) {
    e.preventDefault();
    const journal_uri = individual.id + 'j';
    const journal = new IndividualModel(journal_uri);
    journal.load().then(function (journal) {
      if (!journal.isNew()) {
        riot.route('#/' + journal_uri);
      } else {
        const journalEmpty = new IndividualModel('v-s:JournalEmpty');
        journalEmpty.load().then(function (journalEmpty) {
          alert(journalEmpty.toString());
        });
      }
    });
  });
  $('#send', template).on('click', function (e) {
    BrowserUtil.send(individual, template.parent().closest('[resource]'));
  });
  $('#edit', template).on('click', function (e) {
    if (toRefresh) {
      refresh();
    }
  });
  $('#rights', template).on('click', function () {
    BrowserUtil.showRights(individual);
  });
  $('#files', template).click(async function (e) {
    e.preventDefault();
    const docTemplate = template.parent().closest('[resource]');
    const fileLinks = $("a:has(>span[property='v-s:fileName'])", docTemplate);

    const filesObj = {
      default: [],
      signed: []
    };

    for (let i = 0; i < fileLinks.length; i++) {
      const uri = $(fileLinks.get(i)).closest('[resource]').attr('resource');
      const fileIndivid = await new veda.IndividualModel(uri).load();
      if (fileIndivid.hasValue('v-s:digitalSignature')) {
        filesObj.signed.push(fileIndivid);
      } else if (!fileIndivid.hasValue('v-s:backwardProperty', 'v-s:digitalSignature')){
        filesObj.default.push(fileIndivid);
      }
    }

    const filesPromisesDefault = filesObj.default.map(function (fileIndivid) {
      const fileName = fileIndivid.hasValue('v-s:fileName') ? fileIndivid['v-s:fileName'][0] : 'unknown';
      const fileUrl = `/files/${fileIndivid.id}`;
      return filePromise(fileUrl, fileName);
    });

    let filePromisesSigned = filesObj.signed.map((fileIndivid) => {
      const signsPromises = fileIndivid['v-s:digitalSignature'].map(sign => {
        return sign.load().then(loaded => {
          const fileName = loaded.hasValue('v-s:fileName') ? loaded['v-s:fileName'][0] : 'unknown';
          const fileUrl = `/files/${loaded.id}`;
          return filePromise(fileUrl, fileName);
        });
      });
      const fileName = fileIndivid.hasValue('v-s:fileName') ? fileIndivid['v-s:fileName'][0] : 'unknown';
      const fileUrl = `/files/${fileIndivid.id}`;
      return Promise.all([filePromise(fileUrl, fileName)].concat(signsPromises));
    });

    const allPromises = [Promise.all(filesPromisesDefault)].concat(filePromisesSigned);
    if (allPromises.length == 0) {
      toggleSpin(btn);
      return;
    }

    return Promise.all(allPromises)
      .then(function (allFiles) {
        import('jszip').then(function (module) {
          const JSZip = module.default;
          const zip = new JSZip();
          //const folder = zip.folder('files');
          const unique = {};
          let folderForSigned;
          allFiles.forEach((files, ind) => {
            if (ind > 0) {
              folderForSigned = zip.folder(files[0].name);
            }
            files.forEach(function (file) {
              let name = file.name;
              let i = 1;
              while (unique[name]) {
                name = file.name.replace(/(.*?).([^.]*)$/, '$1 (' + i + ').$2');
                if (name === file.name) {
                  name = file.name + ' (' + i + ')';
                }
                i++;
              }
              file.name = name;
              unique[file.name] = true;
              if (ind == 0) {
                zip.file(file.name, file);
              } else {
                folderForSigned.file(file.name, file);
              }
            });
          });
          zip.generateAsync({type: 'blob'}).then(function (content) {
            import('filesaver').then(function (module) {
              const saveAs = module.default;
              saveAs(content, 'registry.zip');
            });
          });
        });
      })
      .catch(function (error) {
        notify('danger', {message: 'Ошибка выгрузки реестра. Обратитесь в поддержку.'});
      });
  });
  //find error button
  template[0].querySelector('#findError').addEventListener('click', function () {
    const modalContainer = $('.modal.in')[0];
    const container = modalContainer || document.querySelector('#main');
    const errorElements = document.querySelectorAll('.has-error');

    function canScrollIntoView(element) {
      const rect = element.getBoundingClientRect();
      return rect.width > 0;
    }
    for (let errorElement of errorElements) {
      if (errorElements.length > 0 && window.getComputedStyle(errorElement).visibility == 'hidden') {
        if (canScrollIntoView(errorElement)) {
          errorElement.scrollIntoView({ behavior: 'smooth', block: 'center' });
          $(errorElement).popover('show');
          break;
        } else {
          console.log('Элемент не может быть прокручен в поле зрения');
        }
      } else {
        const hiddenErrorSection = $('.section-with-error', container)[0];
        if (hiddenErrorSection) {
          hiddenErrorSection.dispatchEvent(new Event('showRequest'));
          if (errorElement && canScrollIntoView(errorElement)) {
            errorElement.scrollIntoView({ behavior: 'smooth', block: 'center' });
            $(errorElement).popover('show');
            break;
          } else {
            console.log('Элемент не может быть прокручен в поле зрения');
          }
        } else {
          notify('danger', { message: 'Ошибка поиска, обратитесь в службу поддержки.' });
        }
      }
    }
    
  });

  function filePromise (url, name) {
    return new Promise(function (resolve, reject) {
      const xhr = new XMLHttpRequest();
      xhr.open('GET', url + '?' + Date.now(), true);
      xhr.responseType = 'blob';
      xhr.onload = function (e) {
        if (this.status == 200) {
          const file = new Blob([this.response], {type: 'application/octet-stream'});
          file.name = name;
          file.url = url;
          resolve(file);
        } else {
          reject(xhr.statusText);
        }
      };
      xhr.onerror = function () {
        reject(xhr.statusText);
      };
      xhr.send();
    });
  }
  // Standard task
  template.on('click', 'ul#standard-task a', function (e) {
    e.preventDefault();
    const startFormTransform = $(this).attr('about');
    BrowserUtil.send(individual, template, startFormTransform, true);
  });

  // Standard process
  template.on('click', 'ul#standard-process a', function (e) {
    e.preventDefault();
    const processDefinitionId = e.target.getAttribute('about');
    const processDefinition = new IndividualModel(processDefinitionId);
    BrowserUtil.startProcess(processDefinition, individual);
  });

  // var allButtons = "send edit save cancel delete destroy journal task rights";
  const defaultButtons = 'send edit save cancel delete recover journal task';
  return individual.rights.then(function (rights) {
    const canUpdate = rights.hasValue('v-s:canUpdate', true);
    const canDelete = rights.hasValue('v-s:canDelete', true);
    const enabledButtons = (container.data('buttons') || defaultButtons).trim().split(/\s+/);
    enabledButtons.forEach(function (id) {
      if (!canUpdate && (id === 'save' || id === 'edit' || id === 'cancel' || id === 'recover')) {
        return;
      }
      if (!canDelete && (id === 'delete' || id === 'destroy')) {
        return;
      }
      $('#' + id, template).removeClass('rm hidden');
    });
    $('.rm', template).remove();
  });
};

export const post = function (individual, template, container, mode, extra) {
  template = $(template);
  container = $(container);

  async function hideButtonsForDeleted () {
    if (individual.hasValue('v-s:deleted', true)) {
      template.find(':not(#delete, #recover, #refresh, #toggle-actions)').addClass('hidden');
      $('#delete', template).addClass('hidden');
      // #recover not have .rm for autoremove on canUpdate==false
      const canUpdate = await individual.canUpdate();
      if (canUpdate) {
        $('#recover', template).removeClass('hidden');
      }
    } else {
      template.find(':not(#delete, #recover, #refresh, #toggle-actions)').removeClass('hidden');
      $('#delete', template).removeClass('hidden');
      $('#recover', template).addClass('hidden');
    }
  }
  hideButtonsForDeleted();
  individual.on('v-s:deleted', hideButtonsForDeleted);
  template.one('remove', function () {
    individual.off('v-s:deleted', hideButtonsForDeleted);
  });

  $('#toggle-actions', template).click(function () {
    template.closest('.actions').children(':not(#toggle-actions)').toggleClass('hidden');
    $(this).toggleClass('glyphicon-chevron-left glyphicon-chevron-right btn-link btn-info');
  });

  // Make position fixed for buttons bar that doesn't fit the window
  function checkOffset (main, actions, placeholder) {
    const mainTop = main.offset().top;
    const mainHeight = main.height();
    const windowHeight = window.innerHeight;
    const windowTop = window.scrollY || window.pageYOffset;
    const actionsStaticTop = placeholder.offset().top;
    const actionsStaticHeight = actions.height();
    const actions_inside_viewport = windowTop <= actionsStaticTop && actionsStaticTop + actions.height() < windowTop + windowHeight;
    const main_inside_viewport = windowTop <= mainTop + mainHeight - actionsStaticHeight && mainTop + actionsStaticHeight < windowTop + windowHeight;
    if (!actions_inside_viewport && main_inside_viewport) {
      if (!actions.hasClass('actions-fixed')) {
        placeholder.css('height', actionsStaticHeight);
        actions.addClass('actions-fixed');
      }
    } else {
      if (actions.hasClass('actions-fixed')) {
        placeholder.css('height', 0);
        actions.removeClass('actions-fixed');
      }
    }
  }

  const main = template.parent().closest('[resource]');
  const actions = template.closest('.actions');
  let placeholder;
  if (actions.length) {
    placeholder = $('<div></div>').insertBefore(actions);
    $(window).on('scroll', scrollHandler);
    template.one('remove', function () {
      $(window).off('scroll', scrollHandler);
    });
    $('#toggle-actions', template).detach().appendTo(actions).removeClass('hidden');
    setTimeout(checkOffset, 0, main, actions, placeholder);
  }
  function scrollHandler () {
    checkOffset(main, actions, placeholder);
  }

  // Respect validation state of parent template
  const closest = template.parent().closest('[resource]');
  closest.on('internal-validated', function (e) {
    const validation = e.detail;
    if (validation.state) {
      $('.action#save', template).removeAttr('disabled');
      $('.action#send', template).removeAttr('disabled');
      $('.action#task-button', template).removeAttr('disabled');
      $('.action#findError', template).addClass('hidden');
    } else {
      $('.action#save', template).attr('disabled', 'disabled');
      $('.action#send', template).attr('disabled', 'disabled');
      $('.action#task-button', template).attr('disabled', 'disabled');
      $('.action#findError', template).removeClass('hidden');

    }
    e.stopPropagation();
  });
};

export const html = `
  <span>
    <button type="button" class="btn btn-success view -edit -search hidden glyphicon glyphicon-refresh" id="refresh" data-action="refresh"></button>
    <button type="button" class="rm hidden action btn btn-danger -view edit -search" id="findError" data-action="findError" about="v-s:FindErrorBundle" property="rdfs:label"></button>
    <button type="submit" class="rm hidden action btn btn-warning view edit -search" id="send" data-action="send" about="v-s:Send" property="rdfs:label"></button>
    <button type="button" class="rm hidden action btn btn-primary view -edit -search" id="edit" data-action="edit" about="v-s:Edit" property="rdfs:label"></button>
    <button type="submit" class="rm hidden action btn btn-success -view edit -search" id="save" data-action="save" about="v-s:Save" property="rdfs:label"></button>
    <button type="button" class="rm hidden action btn btn-default -view edit -search" id="cancel" data-action="kancel" about="v-s:Cancel" property="rdfs:label"></button>
    <button type="button" class="rm hidden action btn btn-link view -edit -search" id="delete" data-action="delete" about="v-s:Delete" property="rdfs:label"></button>
    <button type="button" class="hidden action btn btn-primary view -edit -search" id="recover" data-action="recover" about="v-s:Recover" property="rdfs:label"></button>
    <button type="button" class="rm hidden action btn btn-danger view edit -search" id="destroy" data-action="destroy" about="v-s:Destroy" property="rdfs:label"></button>
    <button type="button" class="rm hidden action btn btn-default view -edit -search" id="journal" data-action="journal" about="v-s:ViewJournal" property="rdfs:label"></button>
    <button type="button" class="rm hidden action btn btn-default view -edit -search" id="rights" data-action="rights" about="v-s:Rights" property="rdfs:label"></button>
    <button type="button" class="rm hidden action btn btn-default view -edit -search" id="files" data-action="files" about="v-fs:DownloadAllFiles" property="rdfs:label"></button>
    <div class="rm hidden action btn-group dropup view -edit -search" id="task">
      <button class="action btn btn-warning btn-block dropdown-toggle" id="task-button" data-toggle="dropdown">
        <span about="v-s:SendTask" property="rdfs:label"> </span>
        <span class="caret"></span>
      </button>
      <ul class="dropdown-menu" id="standard-task">
        <li><a href="#" about="v-wf:questionRouteStartForm" property="rdfs:label"></a></li>
        <li><a href="#" about="v-wf:instructionRouteStartForm" property="rdfs:label"></a></li>
        <li><a href="#" about="v-wf:taskRouteStartForm" property="rdfs:label"></a></li>
        <li><a href="#" about="v-wf:coordinationRouteStartForm" property="rdfs:label"></a></li>
        <li><a href="#" about="v-wf:signRouteStartForm" property="rdfs:label"></a></li>
        <li><a href="#" about="v-wf:confirmationRouteStartForm" property="rdfs:label"></a></li>
        <li class="divider"></li>
        <li><a href="#" about="v-wf:distributionRouteStartForm" property="rdfs:label"></a></li>
      </ul>
    </div>
    <div class="rm hidden action btn-group dropup view -edit -search" id="process">
      <button class="action btn btn-warning btn-block dropdown-toggle" id="process-button" data-toggle="dropdown">
        <span about="v-ui:StartProcess" property="rdfs:label"></span>
        <span class="caret"></span>
      </button>
      <ul class="dropdown-menu" id="standard-process">
        <li><a href="#" about="bpmn:SimpleTaskProcessDefinition" property="rdfs:label"></a></li>
      </ul>
    </div>
    <button type="button" class="btn btn-link view edit -search action hidden glyphicon glyphicon-chevron-left" id="toggle-actions"></button>
  </span>
`;
