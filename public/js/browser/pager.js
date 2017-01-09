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
})(jQuery );
