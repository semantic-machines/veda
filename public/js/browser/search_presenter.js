// Search Presenter

veda.Module(function SearchPresenter(veda) { "use strict";

  var template = $("#search-template").html();

  // Initialize search page
  veda.on("search:loaded", function (search, container_param, page_param) {

    if (container_param === null) return;

    var container = container_param || $("#main");

    // Clear #main container
    if (container.prop("id") === "main") {
      container.empty();
    }

    container.hide();

    search.currentPage = (typeof page_param === "number" ? page_param : search.currentPage) || 0;

    // Get template
    var rendered = riot.render(template, search);
    container.append(rendered);

    $("#q", container).focus();
    $(".not-found", container).hide();
    $("#search-results", container).hide();

    // Listen View changes & update Model
    $("#search-tab-panel-" + search.id + " [bound]", container).on("change", function() {
      search[this.id] = $(this).val();
    });

    $("#search-tab-panel-" + search.id + " #search-submit", container).on("click", function(e) {
      e.preventDefault();
      $("#search-submit", container).addClass("disabled");
      search.currentPage = 0;

      if (container.prop("id") === "main") riot.route("#/search/" + search.q, false);
      search.search();
    });

    // Listen Model changes & update View
    search.on("property:changed", function(property, value) {
      var $el = $("#search-tab-panel-" + search.id + " #" + property + "[bound]", container);
      if ($el.is("input, textarea, select")) $el.val( value );
      else $el.html( value );
    });

    $("#select-all", container).on("click", function (e) {
      search.toggleAll();
      // Redraw current page
      veda.trigger("search:complete", search, container, search.currentPage);
    });

    container.show();

    search.view = container;

    veda.trigger("search:rendered", search, container);
  });

  // Display search results
  veda.on("search:complete", function (search, container_param, page_param) {

    if (container_param === null) return;

    var rt1, rt2, render_time, gc1, gc2, _get_count, gst1, gst2, _get_summary_time;
    rt1 = Date.now();
    gc1 = get_count;
    gst1 = get_summary_time;

    var container = container_param || $("#main");

    search.currentPage = (typeof page_param === "number" ? page_param : search.currentPage) || 0;

    if (search.results_count < search.currentPage * veda.user.displayedElements)
      search.currentPage = Math.floor(search.results_count / veda.user.displayedElements) + 1 * (search.results_count % veda.user.displayedElements ? 1 : 0) - 1;

    // Show/hide 'results' or 'not found'
    $("#search-submit", container).removeClass("disabled");

    if (!search.q && !search.queryPrefix) {
      $("#q", container).focus();
      $("#search-results", container).hide();
      $(".not-found", container).hide();
      return;
    } else if ((search.q || search.queryPrefix) && !search.results_count) {
      $("#q", container).focus();
      $("#search-results", container).hide();
      $(".not-found", container).show();
      return;
    } else {
      $("#search-tab-panel-" + search.id + " #results-link-"+search.id, container).tab("show");
    }
    $(".not-found", container).hide();
    $("#search-results", container).show();
    $("#search-results-list", container)
      .empty()
      .attr("start", search.currentPage * veda.user.displayedElements + 1);
    $("#pager", container).empty();

    // Show results
    var keys = Object.getOwnPropertyNames(search.results);
    var $timing = $("#timing-" + search.id, container);
    var $query = $("#query-" + search.id, container);
    var $queryText = $("#query-text", $query);
    var $render_time = $("#render_time", $timing);
    var $_get_count = $("#get_count", $timing);
    var $_get_summary_time = $("#get_summary_time", $timing);
    $queryText.text(search.fullQuery);
    $query.show();
    for (var i = search.currentPage * veda.user.displayedElements; i < (search.currentPage + 1) * veda.user.displayedElements && i < search.results_count; i++) {
      (function (i) {
        setTimeout(function () {

          var $li = $("<li/>").appendTo( $("#search-results-list", container) );

          // Select search results
          var $select = $( $("#search-select-template").html() );
          $("input[type='checkbox']", $select).on("click", function (e) {
            search.toggleSelected(i);
          });
          $li.append( $select );

          var search_result = new veda.SearchResultModel(search.results[ keys[i] ], $li);
          if (search_result.id in search.selected) $("input", $select).attr("checked", "checked");

          if (i == search.results_count - 1 || i == (search.currentPage + 1) * veda.user.displayedElements - 1) {
            rt2 = Date.now();
            render_time = rt2 - rt1;
            $render_time.html(render_time);
            gc2 = get_count;
            _get_count = gc2 - gc1;
            $_get_count.html(_get_count);
            gst2 = get_summary_time;
            _get_summary_time = gst2 - gst1;
            $_get_summary_time.html(_get_summary_time);
            $timing.show();
          }

        }, 0);
      }(i));
    }

    // Show pager
    var $pager = $("#pager", container);
    for (var page = 0; page < Math.floor(search.results_count / veda.user.displayedElements) + 1 * (search.results_count % veda.user.displayedElements ? 1 : 0); page++) {
      var $page = $("<li/>")
        .attr("class", page == search.currentPage ? "active" : "")
        .appendTo($pager);
      var $a = $("<a/>", {
        "text" : page + 1,
        "click": (function (page) {
          return function (event) {
            event.preventDefault();
            veda.trigger("search:complete", search, container, page);
          };
        })(page),
        "href" : ""
      }).appendTo($page);
    }

  });

});
