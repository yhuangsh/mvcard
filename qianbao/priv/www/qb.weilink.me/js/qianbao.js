var QB = {};

QB.compute_promo_deduction = function(amt, promo) {
    if (promo == 0)
	return 0;
   
    if (promo < 1)
	return amt*promo;

    if (promo > 1)
	return promo;
};

QB.update_bill = function() {
    var amt0 = parseFloat($("input[name='amt0']").val());
    var rebate = parseFloat($("input[name='rebate']").val());
    var promo_strs = $("option:selected").val().split("|");
    var promo_id = promo_strs[0];
    var promo = parseFloat(promo_strs[1]);
    var points_available = parseFloat($("input[name='points']").val());

    var amt_actual = amt0;
    var points_earned = 0;

    var promo_deduction = QB.compute_promo_deduction(amt0, promo);
    var points_deduction = $("input:checked").length > 0 ? points_available : 0;

    var promo_wasted = false;
    if (amt_actual < promo_deduction) {
	promo_deduction = amt_actual;
	promo_wasted = true;
    }
    
    amt_actual -= promo_deduction;

    if (amt_actual < points_deduction)
	points_deduction = amt_actual;

    amt_actual -= points_deduction;
    
    points_earned = amt_actual * rebate;

    var htmlStr = 
	"账单金额:" + amt0.toFixed(2) + "元<br>" +
	"优惠抵扣:" + promo_deduction.toFixed(2) + "元" + (promo_wasted ? " (消费额 < 优惠幅度)<br>" : "<br>") +
	"积分抵扣:" + points_deduction.toFixed(2) + "元<br>" +
	"实际支付:" + amt_actual.toFixed(2) + "元" + (amt_actual > 0 ? " (使用银联手机支付)<br>" : "<br>") +
	"获得积分:" + points_earned.toFixed(0) + "分 (本次消费后共" + ((points_available - points_deduction)*10 + points_earned) + "分)<br>";
   
    console.log(htmlStr);
    
//    $("input[name='pid']").val(promo_id);
//    $("input[name='promo_discnt']").val(promo_deduction);
//    $("input[name='pts_used']").val(points_deduction);
//    $("input[name='amt_actual']").val(amt_actual);
//    $("input[name='pts_earned']").val(points_earned);

    var pwd = $("input[name='pwd']").val();
    var url = 
	"/pay.yaws?tid="+$("input[name='tid']").val()+
	"&pwd="+pwd+
	"&amt0="+amt0+
	"&pid="+promo_id+
	"&promo_discnt="+promo_deduction+
	"&pts_used="+points_deduction+
	"&amt_actual="+amt_actual+
	"&pts_earned="+points_earned;

    console.log(url);
//    console.log("amt0="+amt0);
//    console.log("pwd="+pwd);

    if (amt0 == 0 || pwd == null || pwd == "") 
	$("a#a-pay-button").attr("href", "#");
    else
	$("a#a-pay-button").attr("href", url);

    $("li#li-bill-summary").html(htmlStr);
};

QB.currency_input_handler = function(e) {
    var keycode = e.which;

    if (e.type == "keydown") {
	// intercept return, left, right, up, down
	if (keycode == 13 || (37 <= keycode && keycode <= 40)) { 
	    e.preventDefault();
	} else if (keycode == 8) {
	    var target = $(e.target);
	    var number = target.val();
	    number = number.substr(0, number.length-1) + "0";
	    number = (parseFloat(number) / 10).toFixed(2);
	    target.val(number);
	    e.preventDefault();
	}
    } else if (e.type == "keypress") {
	if (keycode == 46) {
	    alert("您无需输入小数点，最后两位数字会自动变成\"角\"和\"分\"");
	    e.preventDefault();
	} else if (!(48 <= keycode && keycode <=57)) {
	    alert("请您只输入数字");
	    e.preventDefault();
	} else {
	    var keychar = String.fromCharCode(keycode);
	    var target = $(e.target);
	    var number = (parseFloat(target.val()+keychar) * 10).toFixed(2);
	    target.val(number);
	    e.preventDefault();
	}
    }
    QB.update_bill();
};

QB.pay_button_handler = function(e) {
    var url = $("a#a-pay-button").attr("href");
    if (url == "#") {
	alert("您的账单金额为0，或没有输入万维通密码");
	e.preventDefault();
    }
};

QB.scan_bill = function(e) {
    alert("to scan");
    $.mobile.changePage("/bill.yaws?m="+encodeURIComponent("003")+"&a="+encodeURIComponent("500.00"));
    return;
    window.plugins.barcodeScanner.scan( 
    	function(result) {
	    if (!result.cancelled) {
		if (result.format == "QR_CODE") {
		    var ret = result.text.split("|");
		    if (ret.length == 1) {
			//alert("m="+ret[0]);
			$.mobile.changePage("/bill.yaws?m="+encodeURIComponent(ret[0]));
		    } else if (ret.length == 2) {
			//alert("m="+ret[0]+" a="+ret[1]);
			$.mobile.changePage("/bill.yaws?m="+encodeURIComponent(ret[0])+"&a="+encodeURIComponent(ret[1]));
		    } else {
			alert("wrong format = "+result.text);
		    }
		} else {
		    alert("not a QR CODE = "+result.format);
		}
	    } else {
    		alert("scan cannceled");
	    }
//    	    $.mobile.changePage("pay.html");
    	}, 
    	function(error) {
    	    alert("Scanning failed: " + error);
    	}
    );
};

$(document).on(
    "pageinit",
    "#home-page",
    function(event) {
	$("a#input_merchant_code").click(function() {
	    var m = prompt("请输入商家代码:");
	    if (m != null && m != "")
		$.mobile.changePage("/bill.yaws?m="+encodeURIComponent(m));
	});
	$("a#a-scan-button").click(QB.scan_bill);
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
	    $.mobile.changePage("/auth.yaws?u="+encodeURIComponent(u)+"&p="+encodeURIComponent(p)+"&cb="+encodeURIComponent(cb)+"&cb2="+encodeURIComponent(cb2));
	});
    }
);

$(document).on(
    "pageinit",
    "#bill-page",
    function(event) {
	$("input[name='amt0']")
	    .keypress(QB.currency_input_handler)
	    .keydown(QB.currency_input_handler);
	$("select#sel-promos").change(QB.update_bill);
	$("input[name='if-use-points']").change(QB.update_bill);
	$("input[name='pwd']").keyup(QB.update_bill);
	$("a#a-pay-button").click(QB.pay_button_handler);
	QB.update_bill();
    }
);
    

