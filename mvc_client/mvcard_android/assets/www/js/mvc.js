var MVC = {
    Config: function() {
	var confs = ["config_server_url",
		     "security_server_url",
		     "app_server_url",
		     "saved_login_username",
		     "saved_login_token"];

	MVC.Config.prototype.clear = function() {
	    confs.forEach(function(x) {
		this[x] = null;
	    }, this);
	};
	    
	MVC.Config.prototype.loadLocal = function() {
	    confs.forEach(function(x) {
		this[x] = window.localStorage.getItem("config."+x);
	    }, this);
	    return true;
	};

	MVC.Config.prototype.saveLocal = function() {
	    confs.forEach(function(x) {
		if (null == this[x])
		    window.localStorage.removeItem("config."+x);
		else
		    window.localStorage.setItem("config."+x, this[x]);
	    }, this);
	    return true;
	};

	MVC.Config.prototype.downloadAndMerge = function(url) {
	    var me = this;
	    var ret;

	    if (undefined == url)
		url = this.config_server_url;

	    $.getJSON(url+"?callback=?", 
		      function(data) {
			  $.each(data, 
				 function(k, v) {
				     me[k] = v;
				 });
			  ret = true;
		      }).error(function() {
			  ret = false;
		      });
	    return ret;
	};

	this.clear();
	this.config_server_url = "http://admin.weilink.me:8000/config.yaws";
	this.security_server_url = "http://baomi.weilink.me";
	this.app_server_url = "http://mvc.weilink.me";
	this.loadLocal();	
    },

    Session: function(config) {
	this.config = config;
    }
};



// function MVCSession(config) {
//     var m_session_token = undefined;
//     var m_session_key = undefined;
//     var m_config = undefined;

//     this.getConfig = function(url) {};

//     this.start = function(url) {};
//     this.stop = function() {};

//     this.login = function(username, password, captcha) {};
//     this.logout = function() {};
//     this.register = function(username, password, captcha) {};
//     this.changePassword = function(old_password, new_password, captcha) {};
// }

// function MVCTransaction() {
//     this.create = function(session_token) {};
//     this.
// }




var mvcard = {};

mvcard.city_list = ["上海", "北京", "重庆"];
mvcard.current_city = "上海";

mvcard.setCity = function(city) {
    mvcard.current_city = city;
    $("a.city span.ui-btn-text").each(
	function(index, element) {
	    $(element).html(city);
	}
    );
};

function uppay() {
    window.plugins.upPay.pay(
    	"my_order",
    	function(r) {
    	    alert("ok: "+r);
    	},
    	function(e) {
    	    alert("nok: cancel="+e.cancelled);
    	}
    );
};

function scan_qrcode() {
    window.plugins.barcodeScanner.scan( 
    	function(result) {
    	    // alert("We got a barcode\n" +
    	    // 	  "Result: " + result.text + "\n" +
    	    // 	  "Format: " + result.format + "\n" +
    	    // 	  "Cancelled: " + result.cancelled);
    	    $.mobile.changePage("pay.html");
    	}, 
    	function(error) {
    	    alert("Scanning failed: " + error);
    	}
    );
};

$(document).delegate(
    "#home",
    "pagecreate",
    function(event) {
    }
);

$(document).delegate(
    "#pay",
    "pageinit",
    function(event) {
	$("a.start_uppay").click(uppay);
    }
);

$(document).delegate(
    "#home",
    "pageinit",
    function(event) {
	$.mobile.page.prototype.options.backBtnText = "返回";
	$(".barcode_scanner_trigger").click(scan_qrcode);
	$(".merchant_search_input").change(
	    function() {
		alert("change");
	    }
	);
	mvcard.setCity(mvcard.city_list[0]);
    }
);

$(document).delegate(
    "#featured",
    "pageinit",
    function(event) {
	mvcard.setCity(mvcard.current_city);
    }
);

$(document).delegate(
    "#merchants",
    "pageinit",
    function(event) {
	$(".merchant_search_input").change(
	    function() {
		alert("change");
	    }
	);
	mvcard.setCity(mvcard.current_city);
    }
);

$(document).delegate(
    "#select_city",
    "pagecreate",
    function(event) {
//	alert("create");
	var ul = $(".city_list");
	for (var i in mvcard.city_list)
	    ul.append("<li><a class=\"city_name\">"+mvcard.city_list[i]+"</a></li>");
    }
);

$(document).delegate(
    "#select_city", 
    "pageinit", 
    function(event) {
//	alert("init");
	$(".city_name").each(
	    function(index, element) {
		$(element).click(
		    function() {
			mvcard.setCity($(this).html());
			history.back();
		    }
		);
	    }
	);
    }
);

