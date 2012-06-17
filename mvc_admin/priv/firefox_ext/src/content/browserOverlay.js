
if ("undefined" == typeof(zdp)) {
    var zdp = {};
};

// parseUri 1.2.2
// (c) Steven Levithan <stevenlevithan.com>
// MIT License

function parseUri(str) {
	var	o   = parseUri.options,
		m   = o.parser[o.strictMode ? "strict" : "loose"].exec(str),
		uri = {},
		i   = 14;

	while (i--) uri[o.key[i]] = m[i] || "";

	uri[o.q.name] = {};
	uri[o.key[12]].replace(o.q.parser, function ($0, $1, $2) {
		if ($1) uri[o.q.name][$1] = $2;
	});

	return uri;
};

parseUri.options = {
	strictMode: true,
	key: ["source","protocol","authority","userInfo","user","password","host","port","relative","path","directory","file","query","anchor"],
	q:   {
		name:   "queryKey",
		parser: /(?:^|&)([^&=]*)=?([^&]*)/g
	},
	parser: {
		strict: /^(?:([^:\/?#]+):)?(?:\/\/((?:(([^:@]*)(?::([^:@]*))?)?@)?([^:\/?#]*)(?::(\d*))?))?((((?:[^?#\/]*\/)*)([^?#]*))(?:\?([^#]*))?(?:#(.*))?)/,
		loose:  /^(?:(?![^:@]+:[^:@\/]*@)([^:\/?#.]+):)?(?:\/\/)?((?:(([^:@]*)(?::([^:@]*))?)?@)?([^:\/?#]*)(?::(\d*))?)(((\/(?:[^?#](?![^?#\/]*\.[^?#\/.]+(?:[?#]|$)))*\/?)?([^?#\/]*))(?:\?([^#]*))?(?:#(.*))?)/
	}
};

zdp.parseAddr = function(addr) {
    addr2 = addr.split(":")[1];
    return addr2.split("\u00a0\u00a0")[0];
}

zdp.parsePhone = function(addr) {
    addr2 = addr.split(":")[1];
    return addr2.split("\u00a0\u00a0")[1];
}

zdp.onPageLoad = function(event) {
    if (event.target instanceof HTMLDocument) {  
	doc = event.target;
	if (doc.baseURI.search("dianping\.com") != -1) {
	    search_result = doc.evaluate("//dd[./ul[@class='detail']]", doc, null, XPathResult.ORDERED_NODE_SNAPSHOT_TYPE, null);
	    for (i=0;i<search_result.snapshotLength;i++) {
		dd = search_result.snapshotItem(i);
		e = doc.evaluate("ul[@class='detail']/li[@class='shopname']/a", dd, null, XPathResult.ANY_TYPE, null).iterateNext();		
	    	shopurl = parseUri(e.href);
		shopcode = shopurl.file;
	    	shopname = e.title;
	    	e = doc.evaluate("ul[@class='detail']/li[@class='address']", dd, null, XPathResult.ANY_TYPE, null).iterateNext();
	    	shopaddr = zdp.parseAddr(e.textContent);
		shopphone = zdp.parsePhone(e.textContent);

		gtags = [];
		rtags = [];

		e = doc.evaluate("ul[@class='detail']/li[@class='tags']/a", dd, null, XPathResult.ORDERED_NODE_SNAPSHOT_TYPE, null);
		if (e != null) {
		    for (j=0;j<e.snapshotLength;j++) {
			tag = e.snapshotItem(j);
			flag = parseUri(tag.href).file;
			if (flag[0] == "g") {
		    	    gtags.push(tag.textContent);
			} else if (flag[0] == "r") {
		    	    rtags.push(tag.textContent);
			}
		    }
		    gl=gtags.length; 
		    rl=rtags.length;
		    for (j=gl;j<3;j++) gtags[j] = "undefined";
		    for (j=rl;j<3;j++) rtags[j] = "undefined";
		}

		if (shopaddr == null) shopaddr="undefined";
		if (shopname == null) shopname="undefined";
		if (shopaddr == null) shopaddr="undefined";
		if (shopphone == null) shopphone="undefined";

	    	//alert("http://admin.weilink.me/checkin?id="+shopcode+"i1="+shopname+"i2="+shopaddr+"i3=unknown");
		
	    	ul = doc.evaluate("ul[@class='detail']", dd, null, XPathResult.ANY_TYPE, null).iterateNext();
	    	newNode = doc.createElement("a");
	    	newNode.href = "http://admin.weilink.me:8000/nobodyknows/checkin.yaws?id="+shopcode+"&i1="+shopname+"&i2="+shopaddr+"&i3="+shopphone+"&g1="+gtags[0]+"&g2="+gtags[1]+"&g3="+gtags[2]+"&r1="+rtags[0]+"&r2="+rtags[1]+"&r3="+rtags[2];
	    	newNode.appendChild(doc.createTextNode("Add"));
	    	ul.parentNode.insertBefore(newNode, ul);
	    } 
	}
    }
};

zdp.addHook = function() {
    gBrowser.addEventListener("load", zdp.onPageLoad, true);
    window.alert("turned on");
};

zdp.removeHook = function() {
    gBrowser.removeEventListener("load", zdp.onPageLoad, true);
    window.alert("turned off");
};

/*
<strong class="average"><span class="Price">¥</span>95</strong>
<ul class="detail">
<li class="shopname">
  <a href="/shop/506589?KID=92416" class="BL" title="老山东" kid="92416"  onclick="document.hippo.ext({cl_i:1,cl_a:20,cl_k:92416}).mv('cl_to_s',506589);pageTracker._trackPageview('dp_search_shopname_shanghai');$APClickTracker(92416,20);" target="_blank" >老山东</a>
  <a href="/promo/155029?source=6"  onclick="pageTracker._trackPageview('dp_search_promoinfo1_shanghai')" class="iNpromo" title="每满百抵10、1元特价菜" target="_blank"></a>
  <span class="tip">
    <a href="/bussiness" class="spread" target="_blank"  onclick="pageTracker._trackPageview('dp_search_snippet_biz')">推广</a>
  </span>
  <li class="address"><strong>地址:</strong>
    <a href="/search/category/1/0/r5" class="Black-H">浦东新区</a>
    浦东南路379号金穗大厦6楼&nbsp;&nbsp;68869778
  </li>
</li>
<li class="tags"><strong>标签:</strong>
  <a href="/search/category/1/10/g26483" class="Gray" onclick="pageTracker._trackPageview('dp_search_snippet_tag_shanghai')">鲁菜</a>
  &nbsp;&nbsp;
  <a href="/search/category/1/55/g165" class="Gray" onclick="pageTracker._trackPageview('dp_search_snippet_tag_shanghai')">婚宴酒店</a>
  &nbsp;&nbsp;
  <a href="/search/category/1/0/r801" class="Gray" onclick="pageTracker._trackPageview('dp_search_snippet_tag_shanghai')">陆家嘴</a>
</li>
<li class="info">
  <strong>
    <a href="/promo/155029?source=6"  onclick="pageTracker._trackPageview('dp_search_promoinfo2_shanghai')" class="OL" title="每满百抵10、1元特价菜" target="_blank">优惠:</a>
  </strong>
  <a href="/promo/155029?source=6"  onclick="pageTracker._trackPageview('dp_search_promoinfo3_shanghai')" class="B" title="每满百抵10、1元特价菜" target="_blank">每满百抵10、1元特价菜</a>
</li>
</ul>
*/

/*function getDetail(ul) {
    if (null != ul) {
	var doc = ul.ownerDocument;

	var price_element = ul.previousElementSibling;
	if (null != price_element) {
	    var price = price_element.textContent;
	    
	    var detail = ul.getElementsByClassName("BL")[0];
	    var store_code = detail.href;
	    var store_name = detail.textContent;
	    var store_addr = ul.getElementsByClassName("address")[0].textContent;
	    
	    var newNode = doc.createElement("a");
	    newNode.href = "http://admin.mbcard.com/add_resto?r=";
	    newNode.appendChild(doc.createTextNode("Add"));
	    ul.parentNode.insertBefore(newNode, ul);
	    
	    //window.alert(price + store_code + store_name + store_addr);
	} else {
	    window.alert("missing price_element, call David");
	}
    } else {
	window.alert("empty UL, call David");
    }
}

function onPageLoad(event) {  
    if (event.originalTarget instanceof HTMLDocument) {  
	var win = event.originalTarget.defaultView;  
	if (win.frameElement) {  
	    win = win.top;  
	}  
	doc = win.document;
	
	if (doc.baseURI.length >= 23) {
	    if (doc.baseURI.search("dianping\.com") != -1) {
		var details = doc.getElementsByClassName("detail");
		if (details.length > 0) {
		    for(i=0;i<details.length;i++) {
			getDetail(details[i]);
		    }
		} 
	    }
	}
    }
} */ 




