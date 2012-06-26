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
}

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
}

$(document).delegate(
    "#home",
    "pagecreate",
    function(event) {
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

