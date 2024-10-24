/**
 * Application presenter module
 * @module AppPresenter
 */

import '../browser/check_browser.js';
import '../browser/notification_listener.js';
import '../browser/line_status_listener.js';
import '../browser/show_ttl.js';
import veda from '../common/veda.js';
import riot from '../common/lib/riot.js';
import IndividualModel from '../common/individual_model.js';
import Util from '../common/util.js';
import {delegateHandler, clear, spinnerDecorator} from '../browser/dom_helpers.js';

/**
 * Application presenter
 * @param {Object} manifest - The manifest object for the application
 */
export default function AppPresenter (manifest) {
  /**
   * Localize resources on the page on language change
   * @return {void}
   */
  veda.on('language:changed', () => {
    const resourcesNodes = document.querySelectorAll('[resource], [about]');
    let resources = Array.prototype.map.call(resourcesNodes, (node) => node.getAttribute('about') || node.getAttribute('resource'));
    resources = Util.unique(resources);
    resources.forEach((resource_uri) => {
      const resource = new IndividualModel(resource_uri);
      for (const property_uri in resource.properties) {
        if (property_uri === '@') {
          continue;
        }
        if (resource.properties[property_uri] && resource.properties[property_uri].length && resource.properties[property_uri][0].type === 'String') {
          resource.trigger('propertyModified', property_uri, resource.get(property_uri));
          resource.trigger(property_uri, resource.get(property_uri));
        }
      }
    });
  });

  /**
   * Call router when anchor link is clicked
   * @param {Event} event - The click event
   * @this {Element} - The clicked element
   * @return {void}
   */

  function anchorHandler (event) {
    event.preventDefault();
    const hash = this.getAttribute('href');
    return (hash === window.location.hash ? false : riot.route(hash));
  }
  delegateHandler(document.body, 'click', '[href^=\'#/\']', anchorHandler);

  // Prevent empty links routing
  delegateHandler(document.body, 'click', '[href=\'\']', (event) => event.preventDefault());

  // Router installed flag
  let routerInstalled;

  /**
   * Install router
   * @param {Individual} main - The default individual to route if no hash is present
   * @return {void}
   */
  function installRouter (main) {
    if (routerInstalled) {
      return;
    }

    routerInstalled = true;

    /**
     * Router function
     * @param {string} hash - The route hash
     * @return {void}
     */
    riot.route(spinnerDecorator(async (hash) => {
      if (typeof hash === 'string') {
        const hash_index = hash.indexOf('#');
        if (hash_index >= 0) {
          hash = hash.substring(hash_index);
        } else {
          const mainContainer = document.getElementById('main');
          clear(mainContainer);
          await main.present(mainContainer);
        }
      } else {
        const mainContainer = document.getElementById('main');
        clear(mainContainer);
        await main.present(mainContainer);
      }

      const tokens = decodeURI(hash).slice(2).split('/');
      const uri = tokens[0];
      const container = tokens[1] || '#main';
      const template = tokens[2];
      const mode = tokens[3];
      let extra = tokens[4];
      if (extra) {
        extra = extra.split('&').reduce((acc, pair) => {
          const split = pair.split('=');
          const name = split[0] || '';
          const values = split[1].split('|') || '';
          acc[name] = acc[name] || [];
          values.forEach((value) => acc[name].push(parse(value)));
          return acc;
        }, {});
      }

      if (uri) {
        const individual = new IndividualModel(uri);
        const containerEl = document.querySelector(container);
        clear(containerEl);
        await individual.present(containerEl, template, mode, extra);
        if (!individual.scroll) {
          window.scrollTo(0, 0);
        }
      } else {
        const mainContainer = document.getElementById('main');
        clear(mainContainer);
        await main.present(mainContainer);
      }
    }));
  }

  /**
   * Parse extra params in hash
   * @param {string} value - The value to parse
   * @return {Individual|Date|string|number|boolean|null} - The parsed value
   */
  function parse (value) {
    if (!Number.isNaN(parseFloat(value.split(' ').join('').split(',').join('.')))) {
      return parseFloat(value.split(' ').join('').split(',').join('.'));
    }
    if (!Number.isNaN(Date.parse(value))) {
      return new Date(value);
    }
    if (value === 'true') {
      return true;
    }
    if (value === 'false') {
      return false;
    }
    const individual = new IndividualModel(value);
    if (individual.isSync() && !individual.isNew()) {
      return individual;
    }

    return value || null;
  }

  let starting = false;

  // Triggered in auth
  veda.on('started', spinnerDecorator(async () => {
    if (starting === true) return;
    starting = true;

    // Restore hash from query params after redirect
    const url = new URL(window.location.href);
    const hash = url.searchParams.get('hash');
    if (hash) window.location = new URL(window.location.origin + window.location.pathname + hash);

    const {veda_layout, veda_main, start_url} = manifest;
    const appContainer = document.getElementById('app');
    clear(appContainer);
    if (veda_layout && veda_main && start_url) {
      const layout = new IndividualModel(veda_layout);
      await layout.present(appContainer);
      const main = new IndividualModel(veda_main);
      installRouter(main);
      riot.route(window.location.hash || start_url);
    } else {
      console.log('Incomplete layout params in manifest');
      const layout_param_uri = veda.user.hasValue('v-s:origin', 'ExternalUser') ? 'cfg:LayoutExternal' : 'cfg:Layout';
      const layout_param = await new IndividualModel(layout_param_uri).load();
      const layout = layout_param['rdf:value'][0];
      await layout.present(appContainer);
      const main_param_uri = veda.user.hasValue('v-s:origin', 'ExternalUser') ? 'cfg:MainExternal' : 'cfg:Main';
      const main_param = await new IndividualModel(main_param_uri).load();
      const main = main_param['rdf:value'][0];
      installRouter(main);
      riot.route(window.location.hash);
    }
    starting = false;
  }));
}
