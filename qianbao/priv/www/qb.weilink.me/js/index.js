$(document).on(
    "pageinit",
    "#home-page",
    function(event) {
	$("a#input_merchant_code").click(function() {
	    var m = prompt("请输入商家代码:");
	    if (m != null && m != "")
		$.mobile.changePage("/bill.yaws?m="+encodeURIComponent(m));
	});
    }
);

$(document).on(
    "pageinit",
    "#login-page",
    function(event) {
	$("a#login_button").click(function() {
	    prompt("made");
	    $.mobile.changePage("/auth.yaws?u=abc&p=123&cb=xcy");
	});
    }
);

