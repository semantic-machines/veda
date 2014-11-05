// Veda controls implemented as JQuery plugins
(function( $ ) {

	$.fn.stringControl = function( options ) {
		var opts = $.extend( {}, $.fn.stringControl.defaults, options );

		this.attr("style","border: 1px solid red");

		return this;
	};

	$.fn.stringControl.defaults = {
	};

})( jQuery );
