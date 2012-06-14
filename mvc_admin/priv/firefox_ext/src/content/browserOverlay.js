/**
 * XULSchoolChrome namespace.
 */
if ("undefined" == typeof(XULSchoolChrome)) {
  var XULSchoolChrome = {};
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

function getDetail(ul) {
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
}  

/**
 * Controls the browser overlay for the Hello World extension.
 */
XULSchoolChrome.BrowserOverlay = {
  /**
   * Says 'Hello' to the user.
   */
  sayHello : function(aEvent) {
    gBrowser.addEventListener("load", onPageLoad, true);
    window.alert("installed");
  }
};
