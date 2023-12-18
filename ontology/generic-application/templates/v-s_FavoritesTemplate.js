import IndividualModel from '/js/common/individual_model.js';
import {delegateHandler, clear} from '/js/browser/dom_helpers.js';

export const pre = async function (individual, template, container, mode, extra) {
  setDragDrop(template);

  delegateHandler(template, 'click', '.content .delete', async function (e) {
    e.preventDefault();
    const parentUri = (this.closest('.folder-content') ?? this.closest('.favorites-list')).getAttribute('resource');
    const parent = new IndividualModel(parentUri);
    const resourceUri = this.closest('[resource]').getAttribute('resource');
    const resource = new IndividualModel(resourceUri);
    try {
      await parent.removeValue(undefined, resource);
      await parent.save();
      await resource.remove();
    } catch (error) {
      console.log(error);
      alert('Ошибка удаления объекта: ' + error.message);
    }
  }, true);

  delegateHandler(template, 'click', '.folder-name', async function (e) {
    const folderUri = this.getAttribute('about');
    individual['v-s:chosenFavoriteFolder'] = folderUri;
    await individual.save();
  }, true);
};

export const post = async function (individual, template, container, mode, extra) {
  const contentsEl = template.querySelector('.content');
  async function chosenFolderHandler () {
    clear(contentsEl);
    template.querySelector('.folder-name.active')?.classList.remove('active');
    let chosen;
    if (!individual.hasValue('v-s:chosenFavoriteFolder')) {
      chosen = individual['v-s:hasFavoriteFolder'][0];
    } else {
      chosen = individual['v-s:chosenFavoriteFolder'][0];
    }
    await chosen.present(contentsEl, folderTemplate);
    template.querySelector(`.folder-name[about="${chosen.id}"]`)?.classList.add('active');
  }
  individual.on('v-s:chosenFavoriteFolder', chosenFolderHandler);
  template.addEventListener('remove', () => individual.off('v-s:chosenFavoriteFolder', chosenFolderHandler));
  await chosenFolderHandler();
};

const folderTemplate = `
  <ol rel="v-s:hasItem" class="folder-content">
    <li class="padding-md" style="border-bottom:1px dotted #eee;" draggable="true">
      <div style="display:flex;">
        <div style="flex:auto;" about="@" rel="v-s:onDocument">
          <a style="display:block;" href="#/@" about="@" data-template="v-ui:ClassNameLabelTemplate" draggable="false"></a>
        </div>
        <div class="dropdown pull-right">
          <a href="" style="text-decoration:none; color:dimgray;" class="options pull-right glyphicon glyphicon-option-vertical dropdown-toggle" data-toggle="dropdown"></a>
          <ul class="dropdown-menu">
            <li><a class="delete" href="#">Удалить</a></li>
          </ul>
        </div>
      </div>
    </li>
  </ol>
`;

function setDragDrop (template) {
  delegateHandler(template, 'dragstart', '[resource][draggable="true"]', dragstartHandler);

  delegateHandler(template, 'dragenter', '.droppable', dragenterHandler);
  delegateHandler(template, 'dragover', '.droppable', dragenterHandler);

  delegateHandler(template, 'dragend', '[resource][draggable="true"]', dragendHandler);
  delegateHandler(template, 'drop', '.droppable', dragendHandler);
  delegateHandler(template, 'drop', '.droppable', dropHandler);

  function dragenterHandler (e) {
    if (e.dataTransfer.types.includes('veda/uri')) {
      e.preventDefault();
    }
  }

  function dragstartHandler (e) {
    [...template.querySelectorAll('.droppable')].forEach((droppable) => droppable.style.outline='2px solid #eee');
    const resource = this.getAttribute('resource');
    const relEl = this.closest('[rel]');
    if (relEl) {
      const rel = relEl.getAttribute('rel');
      e.dataTransfer.setData('veda/rel', rel);
      const sourceEl = relEl.closest('[resource]');
      if (sourceEl) {
        const source = sourceEl.getAttribute('resource');
        e.dataTransfer.setData('veda/source', source);
      }
    }
    e.dataTransfer.setData('veda/uri', resource);
    e.dataTransfer.effectAllowed = 'move';
  }

  function dragendHandler (e) {
    [...template.querySelectorAll('.droppable')].forEach((droppable) => droppable.style.outline='none');
  }

  async function dropHandler (e) {
    const subscriptionUri = e.dataTransfer.getData('veda/uri');
    const subscription = new IndividualModel(subscriptionUri);
    if (!subscription.hasValue('rdf:type', 'v-s:Subscription')) return;

    const sourceUri = e.dataTransfer.getData('veda/source');
    const source = new IndividualModel(sourceUri);
    const sourceRel = e.dataTransfer.getData('veda/rel');
    const targetUri = e.target.closest('[resource]').getAttribute('resource');
    const target = new IndividualModel(targetUri);
    if (source === target) return;
    if (target.hasValue('rdf:type', 'v-s:Folder')) {
      if (!target.hasValue('v-s:hasItem', subscription)) {
        const values = target.get('v-s:hasItem');
        values.unshift(subscription);
        target.set('v-s:hasItem', values);
      }
    } else if (target.hasValue('rdf:type', 'v-s:PersonalAspect')) {
      if (!target.hasValue('v-s:hasFavorite', subscription)) {
        const values = target.get('v-s:hasFavorite');
        values.unshift(subscription);
        target.set('v-s:hasFavorite', values);
      }
    }
    await source.removeValue(sourceRel, subscription);
    try {
      await target.save();
      await source.save();
    } catch (error) {
      console.log(error);
      alert('Ошибка записи объекта: ' + error.message);
    }
  }
}

export const html = `
<div class="container">
  <h4 class="text-center" style="text-transform: uppercase">
    <i class="fa fa-star text-muted margin-md-h"></i><span about="v-s:Favorites" property="rdfs:label"></span>
  </h4>
  <hr class="margin-lg" />
  <div class="row">
    <div class="col-lg-4 col-12">
      <div class="folders-list sheet">
        <ul style="margin:0;" class="folders-tree" about="@" rel="v-s:hasFavoriteFolder" data-template="v-s:FavoriteFolderTreeTemplate"></ul>
      </div>
    </div>
    <div class="col-lg-8 col-12 sheet">
      <div class="content"></div>
      <i class="text-muted margin-xl-h"><small>Избранные документы можно перемещать по спискам, используя перетаскивание</small></i>
    </div>
  </div>
</div>
`;