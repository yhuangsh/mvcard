
var UPPay = function() {
};

UPPay.prototype.pay = function(order, success, failure) {
    cordova.exec(success, failure, 'UPPay', 'pay', [order]);
};

cordova.addConstructor(function() {
    cordova.addPlugin('upPay', new UPPay());
});
