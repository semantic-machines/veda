var STATIC = 'static-2';
var API = 'api-1';

this.addEventListener('activate', function(event) {
  var cacheWhitelist = [ STATIC, API ];

  event.waitUntil(
    caches.keys().then(function(keyList) {
      return Promise.all(keyList.map(function(key) {
        if (cacheWhitelist.indexOf(key) === -1) {
          return caches.delete(key);
        }
      }));
    })
  );
});

this.addEventListener('install', function(event) {
  event.waitUntil(
    caches.open( STATIC ).then(function(cache) {
      //Cache static resources
      return cache.addAll([
        // Index
        '/',
        '/favicon.ico',
        '/index.html',
        '/ontology.json',

        // Styles
        '/css/bootstrap.min.css',
        '/css/codemirror/codemirror.css',
        '/css/codemirror/fullscreen.css',
        '/css/fullcalendar.min.css',
        '/css/bootstrap-datetimepicker.min.css',
        '/css/veda.css',
        '/css/font-awesome.min.css',

        // Utils & UI
        '/js/common/app_model.js',
        '/js/browser/lib/promise.js',
        '/js/browser/lib/jquery.js',
        '/js/browser/lib/jquery-ui.min.js',
        '/js/browser/lib/bootstrap.min.js',
        '/modules/docflow/js/browser/lib/jsplumb.js',
        '/js/browser/lib/vis.min.js',
        '/js/browser/lib/bootstrap-contextmenu.js',
        '/js/browser/lib/n3-browser.min.js',
        '/js/browser/lib/FileSaver.min.js',
        '/js/browser/lib/autosize.min.js',
        '/js/browser/lib/moment-with-locales.min.js',
        '/js/browser/lib/bootstrap-datetimepicker.min.js',
        '/js/browser/lib/fullcalendar.min.js',
        '/js/browser/lib/locale/ru.js',
        '/js/browser/lib/jquery.touchSwipe.min.js',
        '/js/browser/lib/jszip.min.js',
        '/js/browser/lib/marked.min.js',

        // CodeMirror
        '/js/browser/lib/codemirror/codemirror.js',
        '/js/browser/lib/codemirror/mode/xml/xml.js',
        '/js/browser/lib/codemirror/mode/javascript/javascript.js',
        '/js/browser/lib/codemirror/mode/css/css.js',
        '/js/browser/lib/codemirror/mode/turtle/turtle.js',
        '/js/browser/lib/codemirror/mode/htmlmixed/htmlmixed.js',
        '/js/browser/lib/codemirror/addon/edit/closebrackets.js',
        '/js/browser/lib/codemirror/addon/edit/closetag.js',
        '/js/browser/lib/codemirror/addon/edit/matchbrackets.js',
        '/js/browser/lib/codemirror/addon/edit/matchtags.js',
        '/js/browser/lib/codemirror/addon/fold/xml-fold.js',
        '/js/browser/lib/codemirror/addon/display/fullscreen.js',

        // Veda browser & server
        '/js/common/lib/riot.js',
        '/js/common/lib/sha256.js',
        '/js/common/veda_spa.js',
        '/js/common/util.js',
        '/js/common/app_model.js?v=3',
        '/js/common/individual_model.js',
        '/js/common/user_model.js?v=1',
        '/js/common/ontology_model.js',
        '/js/common/numerator.js',

        // Veda browser only
        '/js/browser/update_service.js',
        '/js/browser/notify.js',
        '/js/browser/local_db.js?v=1',
        '/js/browser/backend.js',
        '/js/browser/util.js',
        '/js/browser/veda_controls.js',
        '/js/browser/table_sortable.js',
        '/js/browser/individual_presenter.js',
        '/modules/docflow/js/browser/veda_workflow_editor.js',
        '/js/browser/app_presenter.js',
        '/js/browser/auth.js?v=8',

        // Fonts
        '/fonts/FontAwesome.otf',
        '/fonts/fontawesome-webfont.eot',
        '/fonts/fontawesome-webfont.svg',
        '/fonts/fontawesome-webfont.ttf?v=4.7.0',
        '/fonts/fontawesome-webfont.woff?v=4.7.0',
        '/fonts/fontawesome-webfont.woff2?v=4.7.0',
        '/fonts/glyphicons-halflings-regular.eot',
        '/fonts/glyphicons-halflings-regular.svg',
        '/fonts/glyphicons-halflings-regular.ttf',
        '/fonts/glyphicons-halflings-regular.woff',
        '/fonts/glyphicons-halflings-regular.woff2'
      ]);
    })
  );
});

var api_fns = {
  // GET
  'authenticate':'{"end_time":' + (Date.now() + 12 * 3600 * 1000) + ',"id":"","result":200,"user_uri":""}',
  'get_ticket_trusted':'',
  'is_ticket_valid':'true',
  'get_rights':'{"@":"_","rdf:type":[{"data":"v-s:PermissionStatement","type":"Uri"}],"v-s:canCreate":[{"data":true,"type":"Boolean"}],"v-s:canDelete":[{"data":false,"type":"Boolean"}],"v-s:canRead":[{"data":true,"type":"Boolean"}],"v-s:canUpdate":[{"data":true,"type":"Boolean"}]}',
  'get_rights_origin':'',
  'get_membership':'{"@":"_","rdf:type":[{"data":"v-s:Membership","type":"Uri"}],"v-s:memberOf":[{"data":"v-s:AllResourcesGroup","type":"Uri"}]}',
  'get_individual':'{"@": "$$$","rdf:type":[{"type":"Uri","data": "rdfs:Resource"}],"rdfs:label": [{"type": "String", "data": "Вы работаете офлайн. Этот объект сейчас недоступен.", "lang": "RU"},{"type": "String", "data": "You are offline. This object is not available now.", "lang": "EN"}]}',
  'reset_individual':'{"@": "$$$","rdf:type":[{"type":"Uri","data": "rdfs:Resource"}],"rdfs:label": [{"type": "String", "data": "Вы работаете офлайн. Этот объект сейчас недоступен.", "lang": "RU"},{"type": "String", "data": "You are offline. This object is not available now.", "lang": "EN"}]}',

  // POST
  'query':'{"result":[],"count":0,"estimated":0,"processed":0,"cursor":0,"result_code":200}',
  'get_individuals':'[]',

  // PUT
  'remove_individual':'{"op_id":0,"result":200}',
  'put_individual':'{"op_id":0,"result":200}',
  'add_to_individual':'{"op_id":0,"result":200}',
  'set_in_individual':'{"op_id":0,"result":200}',
  'remove_from_individual':'{"op_id":0,"result":200}',
  'put_individuals':'{"op_id":0,"result":200}'
};

var re = /.*\/\/[^\/]*\/([^\/#?]*)/;

this.addEventListener('fetch', function(event) {
  var fn = event.request.url.match(re)[1];
  var isApi = fn in api_fns;
  event.respondWith( isApi ? getApiResponse(event, fn) : getStaticResource(event) );
});

function getStaticResource(event) {
  return caches.match(event.request).then(function(resp) {
    return resp || fetch(event.request).then(function(response) {
      return caches.open( STATIC ).then(function(cache) {
        cache.put(event.request, response.clone());
        return response;
      });
    });
  });
}

function getApiResponse(event, fn) {
  return fetch(event.request).then(function(response) {
    if (event.request.method === "GET") {
      return caches.open( API ).then(function(cache) {
        cache.put(event.request, response.clone());
        return response;
      });
    }
    return response;
  }).catch(function (err) {
    return caches.match(event.request).then(function (match) {
      if (match) {
        return match;
      } else {
        return new Response(api_fns[fn], { headers: { 'Content-Type': 'application/json' } });
      }
    });
  });
}
