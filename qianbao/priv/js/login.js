$(document).bind(
    "pageinit",
    function(event) {
	alert("fuck");
	$("a#login_button").click(function() {
	    prompt("made");
	    $.mobile.changePage("/auth.yaws?u=abc&p=123&cb=xcy");
	});
    }
);
