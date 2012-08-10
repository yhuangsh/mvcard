package com.qianbao;

import org.apache.cordova.CordovaChromeClient;
import org.apache.cordova.CordovaWebView;
import org.apache.cordova.CordovaWebViewClient;
import org.apache.cordova.DroidGap;

import android.app.Activity;
import android.content.Intent;
import android.content.SharedPreferences;
import android.net.Uri;
import android.os.Bundle;
import android.util.Log;
import android.webkit.WebView;

public class MainActivity extends DroidGap {
	private static final String TAG = "MainActivity";
	private static final String BASEURL = "http://qb.weilink.me:8000";
	
	private static final int REQ_BARCODESCAN = 0x0cafe001;
	private static final int REQ_UPPAY = 0x0cafe002;
	
	private static final String PREFS_NAME = "qianbao";
	private static final String PREF_LAST_URL = "last_url";
	private static final String PREF_CALLBACK_URL = "callback_url";
	
    @Override
    public void onCreate(Bundle savedInstanceState) { 
        super.onCreate(savedInstanceState);
        
        SharedPreferences sp = getSharedPreferences("QIANBAO", MODE_PRIVATE);
        String url = sp.getString(PREF_LAST_URL, BASEURL+"/index.yaws");
 
        super.clearCache();
        super.setIntegerProperty("splashscreen", R.drawable.splash);
       	super.loadUrl(url, 5000);
    }

	@Override
	public void onDestroy() {
		if (!isFinishing()) {
			Log.d(TAG, "onDestroy, saving current url");
			SharedPreferences.Editor spe = getSharedPreferences(PREFS_NAME, MODE_PRIVATE).edit();
			spe.putString("URL", appView.getUrl());
			spe.commit();
		}
		super.onDestroy();
	}

	
	@Override
	protected void onActivityResult(int requestCode, int resultCode, Intent intent) {
		super.onActivityResult(requestCode, resultCode, intent);
		Log.d(TAG, "on activity result");
		switch (requestCode) {
		case REQ_BARCODESCAN:
			if (resultCode == Activity.RESULT_OK) {
				Log.d(TAG, "scan ok");
				String code = intent.getStringExtra("SCAN_RESULT");
				SharedPreferences sp = getSharedPreferences(PREFS_NAME, MODE_PRIVATE);
				String cb_url = sp.getString(PREF_CALLBACK_URL, null);
				if (cb_url == null) {
					Log.d(TAG, "null callback url, do nothing");
				} else {
					String[] strs = code.split("|");
					String url = null;
					if (strs.length == 1) {
						url = cb_url+"?m="+strs[0];
					} else if (strs.length == 2) {
					 	url = cb_url+"?m="+strs[0]+"&a="+strs[1];
					} else {
						Log.d(TAG, "wrong format");
						return;
					}
					Log.d(TAG, "url="+url);
					
				}
			} else {
				Log.d(TAG, "scan nok, do nothing");
			}
			break;
		case REQ_UPPAY:
			if (resultCode == Activity.RESULT_OK) {
				Log.d(TAG, "uppay ok");
			}
			break;
		default:
			break;
		}
	} 

	@Override
	public void init() {
		Log.d(TAG, "my init()");
		CordovaWebView webView = new CordovaWebView(this);
		this.init(
				webView, 
				new CordovaWebViewClient(this, webView) {
					public boolean shouldOverrideUrlLoading(final WebView view, String url) {
						Log.d(TAG, "looking at qianbao scheme");
						
						Uri parsed_url = Uri.parse(url);
						if (parsed_url.getScheme().equals("qianbao")) {
							String func = parsed_url.getHost();
							String action = parsed_url.getLastPathSegment();
							
							if (func.equals("barcode")) {
								if (action.equals("scan")) {
									String callback_url = parsed_url.getQueryParameter("cb");
									Log.d(TAG, "barcode/scan cb="+callback_url);
									SharedPreferences.Editor spe = getSharedPreferences(PREFS_NAME, MODE_PRIVATE).edit();
									spe.putString(PREF_CALLBACK_URL, callback_url);
									spe.commit();
									Intent intent = new Intent("com.google.zxing.client.android.SCAN");
							        intent.putExtra("SCAN_MODE", "QR_CODE_MODE");
							        startActivityForResult(intent, REQ_BARCODESCAN);
								} else {
									Log.d(TAG, "unknow action under barcode="+action);
									return false;
								}
							} else if (func.equals("uppay")) {
								
							} else {
								Log.d(TAG, "unknow schema="+url);
								return false;
							}
							
							return true;
						} else {
							Log.d(TAG, "normal url scheme, pass on to phone gap");
							return false;
						}
					}
				}, 
				new CordovaChromeClient(this, webView));
	}
    
}
