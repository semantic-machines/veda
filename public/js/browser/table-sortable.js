// Pagination plugin
"use strict";
(function( $ ) {
  $.fn.tableSortable = function () {
    return this.each(function() {
      var table = $(this);
      var tbody = table.children("tbody");
      var thead = table.children("thead");
      var th = thead.find("th");
      var rows = tbody.children();

      th.each( function () {
        var th = $(this);
        var a = $("<a class='text-muted glyphicon glyphicon-sort-by-attributes'></a>");
        th.prepend(a, " ");
        var index = th.index();

        a.click(function (e) {
          e.preventDefault();
          e.stopPropagation();
          var $this = $(this);
          table.find("thead th a").addClass("text-muted");
          var dir = $this.hasClass("glyphicon-sort-by-attributes-alt") ? "asc" : "desc";
          $this.removeClass("text-muted").toggleClass("glyphicon-sort-by-attributes glyphicon-sort-by-attributes-alt");
          if (dir === "asc") {
            rows.sort(orderAsc);
          } else {
            rows.sort(orderDesc);
          }
          rows.detach().appendTo(tbody);
        });

        function orderAsc (a, b) {
          var valueA = $(a).children(":nth-child(" + (index + 1) + ")").text().toLowerCase().trim();
          var valueB = $(b).children(":nth-child(" + (index + 1) + ")").text().toLowerCase().trim();
          return (valueA < valueB ? -1 : 1);
        }

        function orderDesc (a, b) {
          var valueA = $(a).children(":nth-child(" + (index + 1) + ")").text().toLowerCase().trim();
          var valueB = $(b).children(":nth-child(" + (index + 1) + ")").text().toLowerCase().trim();
          return (valueA > valueB ? -1 : 1);
        }
      });
    });
  };
})(jQuery );
