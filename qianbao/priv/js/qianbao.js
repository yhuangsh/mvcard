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
	    var u = $("input[name=u]").val();
	    var p = $("input[name=p]").val();
	    var cb = $("input[name=cb]").val();
	    var cb2 = $("input[name=cb2]").val();
	    $.mobile.changePage("/auth.yaws?u="+u+"&p="+p+"&cb="+cb+"&cb2="+cb2);
	});
    }
);

