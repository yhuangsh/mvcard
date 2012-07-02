
package com.phonegap.plugins.uppay;

import java.io.IOException;
import java.io.InputStreamReader;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLConnection;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.os.Bundle;
import android.util.Log;

import org.apache.cordova.api.CordovaInterface;
import org.apache.cordova.api.Plugin;
import org.apache.cordova.api.PluginResult;

public class UPPay extends Plugin {
	private static final String PAY_DATA = "PayData";
	private static final String SP_ID = "SpId";
	private static final String SECURITY_CHIP_TYPE = "SecurityChipType";
	private static final String SYS_PROVIDE = "SysProvide";
	private static final String USE_TEST_MODE = "UseTestMode";

	private final static char[] str = new char[2048];
	
	private String callback_id;
	
	public UPPay() {
    }

	public void process_pay_result(String pay_result) {
		if ("success".equals(pay_result)) {
			Log.d(this.getClass().getName(), "fire pay result success");
			this.success(new PluginResult(PluginResult.Status.OK), this.callback_id);
		} else {
			JSONObject obj = new JSONObject();
			if ("fail".equals(pay_result)) {
				Log.d("UUPAY", "result=fail");
				try {
					obj.put("cancelled", false);
				} catch(JSONException e) {
				}
			} else if ("cancel".equals(pay_result)) {
				Log.d("UUPAY", "result=cancel");
				try {
					obj.put("cancelled", true);
				} catch(JSONException e) {
				}				
			} else {
				Log.d("UUPAY", "result="+pay_result);
			}
			this.error(new PluginResult(PluginResult.Status.ERROR, obj), this.callback_id);
		} 
	}
	
	@Override
	public void setContext(CordovaInterface ctx) {
		super.setContext(ctx);
		
		Log.d("UUPay0", "registering pay result receiver...");
		ctx.getContext().registerReceiver(new BroadcastReceiver() {
			public void onReceive(Context context, Intent intent) {
				Log.d("UUPAY0", "pay result receiver fired");
		        process_pay_result(intent.getStringExtra("PayResult"));
			}		
		}, new IntentFilter("com.unionpay.uppay.payResult"));
	}
		
    public PluginResult execute(String action, JSONArray args, String callbackId) {
    	Log.d("UUPAY", "uppay entering");
    	
    	this.callback_id = callbackId;
    	
        if ("pay".equals(action)) {
        	try {
				String order = args.getString(0);
				//URL url = new URL("http://218.80.192.213:1725/getPaa?id=1");
				URL url = new URL("http://admin.weilink.me:8000/order.yaws?ltok=abc&amt=15000&to=merchant1");
				URLConnection conn = url.openConnection();
				InputStreamReader in = new InputStreamReader(conn.getInputStream(), "UTF-8");
				int length = in.read(str, 0, 2048);
				if (length < 2048) {
					String payData = new String(str, 0, length);
					
					Log.d("UUPAY", "getting pay data" + payData);
					
					Bundle bundle = new Bundle();
				
					bundle.putBoolean(USE_TEST_MODE, true);
					bundle.putString(SP_ID, "0009");
					bundle.putString(SECURITY_CHIP_TYPE, null);
					bundle.putString(SYS_PROVIDE, "00000001");
					bundle.putString(PAY_DATA, payData);
	            
					Intent startIntent = new Intent();
					startIntent.putExtras(bundle);
					startIntent.setClassName("com.unionpay.uppay", 
										     "com.unionpay.uppay.PayActivity");
					this.ctx.startActivity(startIntent);
					PluginResult r = new PluginResult(PluginResult.Status.NO_RESULT);
					r.setKeepCallback(true);
					return r;
				} else {
					return new PluginResult(PluginResult.Status.ERROR, "length: "+length);
				}
			} catch (JSONException e) {
				return new PluginResult(PluginResult.Status.JSON_EXCEPTION);
			} catch (MalformedURLException e) {
				return new PluginResult(PluginResult.Status.MALFORMED_URL_EXCEPTION);			
			} catch (IOException e) {
				return new PluginResult(PluginResult.Status.IO_EXCEPTION);
			}
        } else {
            return new PluginResult(PluginResult.Status.INVALID_ACTION);
        }
    }
   
}