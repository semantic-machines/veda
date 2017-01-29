// Pagination plugin
"use strict";
(function( $ ) {
  $.fn.pager = function (options) {
    var opts = $.extend({
      pages: 10,
      active: 1,
      neighbours: 2,
      click: function (page) {
        console.log("page:",  page);
      }
    }, options);
    return this.each(function() {
      var pager = $(this);
      var update = opts.pages <= 20 ? function (page) {
        pager.empty();
        for (var i = 1; i <= opts.pages; i++) {
          pager.append( $("<li" + (i === page ? " class='active'" : "") + "><a href=''>" + i + "</a></li>").data("page", i) );
        }
        pager.children().click( function (e) {
          e.preventDefault();
          var page = $(this).data("page");
          update(page);
          opts.click(page);
        });
      } : function (page) {
        pager.empty();
        $("<li class='active'><a href=''>" + page + "</a></li>").data("page", page).appendTo(pager);
        for (var i = 1; i <= opts.neighbours; i++) {
          var nextPage = page < opts.pages - i + 1 ? page + i : (page + i) % opts.pages;
          $("<li><a href=''>" + nextPage + "</a></li>").data("page", nextPage).appendTo(pager);
          var prevPage = page > i ? page - i : opts.pages - i + 1;
          $("<li><a href=''>" + prevPage + "</a></li>").data("page", prevPage).prependTo(pager);
        }
        $("<li><a href=''><span>1</span><span class='glyphicon glyphicon-backward'></span></a></li>").data("page", 1).prependTo(pager);
        $("<li><a href=''><span class='glyphicon glyphicon-forward'></span><span>" + opts.pages + "</span></a></li>").data("page", opts.pages).appendTo(pager);
        pager.children().click( function (e) {
          e.preventDefault();
          var page = $(this).data("page");
          update(page);
          opts.click(page);
        });
      }
      update(opts.active);
    });
  };
  
  $.fn.pagerPN = function (options) {
    var opts = $.extend({
      pageSize: 10,
      pages: 1,
      active: 1,
      neighbours: 2,
      click: function (page) {
        console.log("page:",  page);
      }
    }, options);
    return this.each(function() {
      var pager = $(this);
      var update = function (page) {
        pager.empty();
        var next = opts.pages - page >= opts.neighbours ? opts.neighbours : opts.pages - page;
        var prev = page > opts.neighbours ? opts.neighbours : page - 1;
        $("<li class='active'><a href=''>" + page + "</a></li>").data("page", page).appendTo(pager);
        for (var n = 1; n <= next; n++) {
          $("<li><a href=''>" + (page + n) + "</a></li>").data("page", page + n).appendTo(pager);
        }
        for (var p = 1; p <= prev; p++) {
          $("<li><a href=''>" + (page - p) + "</a></li>").data("page", page - p).prependTo(pager);
        }
        var forward = page + 1 <= opts.pages ? page + 1 : page;
        var back = page - 1 > 0 ? page - 1 : page ;
        $("<li><a href=''><span class='glyphicon glyphicon-backward'></span> <span>" + (new veda.IndividualModel("v-s:Back")) + "</span></a></li>")
          .data("page", page - 1 > 0 ? page - 1 : 1)
          .addClass(back === page ? "disabled" : "")
          .prependTo(pager);
        $("<li><a href=''><span>" + (new veda.IndividualModel("v-s:Forward")) + "</span> <span class='glyphicon glyphicon-forward'></span></a></li>")
          .data("page", page + 1 < opts.pages ? page + 1 : opts.pages)
          .addClass(forward === page ? "disabled" : "")
          .appendTo(pager);
        pager.children().click( function (e) {
          e.preventDefault();
          var page = $(this).data("page");
          update(page);
          opts.click(page);
        });
      }
      update(opts.active);
    });
  };
})(jQuery );
