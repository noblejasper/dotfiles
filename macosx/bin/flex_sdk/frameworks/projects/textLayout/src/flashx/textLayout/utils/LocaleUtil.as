////////////////////////////////////////////////////////////////////////////////
//
// ADOBE SYSTEMS INCORPORATED
// Copyright 2007-2010 Adobe Systems Incorporated
// All Rights Reserved.
//
// NOTICE:  Adobe permits you to use, modify, and distribute this file 
// in accordance with the terms of the license agreement accompanying it.
//
////////////////////////////////////////////////////////////////////////////////
package flashx.textLayout.utils
{
	import flash.text.engine.JustificationStyle;
	import flash.text.engine.TextBaseline;
	import flash.utils.Dictionary;
	
	import flashx.textLayout.debug.assert;
	import flashx.textLayout.formats.JustificationRule;
	import flashx.textLayout.formats.LeadingModel;
	import flashx.textLayout.tlf_internal;
	
	
	use namespace tlf_internal;
	/** 
	 * Utilities for managing and getting information about Locale based defaults.
	 * The methods of this class are static and must be called using
	 * the syntax <code>LocaleUtil.method(<em>parameter</em>)</code>.
	 *
	 * @playerversion Flash 10
	 * @playerversion AIR 1.5
	 * @langversion 3.0
	 */
	[ExcludeClass]
	public final class LocaleUtil
	{
		public function LocaleUtil()
		{
		}
		
		
		/** @private */
		static private var _localeSettings:Dictionary = null;
		static private var _lastLocaleKey:String = "";
		static private var _lastLocale:LocaleSettings = null;
		
		static public function justificationRule(locale:String):String 
		{
			var localeSet:LocaleSettings = fetchLocaleSet(locale);
			CONFIG::debug{ assert(localeSet != null, "Failed to get LocaleSettings from locale! locale = " + locale); }
			
			return localeSet.justificationRule;
		}
		
		static public function justificationStyle(locale:String):String
		{
			var localeSet:LocaleSettings = fetchLocaleSet(locale);
			CONFIG::debug{ assert(localeSet != null, "Failed to get LocaleSettings from locale! locale = " + locale); }
			
			return localeSet.justificationStyle;
		}
		
		static public function leadingModel(locale:String):String
		{
			var localeSet:LocaleSettings = fetchLocaleSet(locale);
			CONFIG::debug{ assert(localeSet != null, "Failed to get LocaleSettings from locale! locale = " + locale); }
			
			return localeSet.leadingModel;
		}
		
		
		static public function dominantBaseline(locale:String):String
		{
			var localeSet:LocaleSettings = fetchLocaleSet(locale);
			CONFIG::debug{ assert(localeSet != null, "Failed to get LocaleSettings from locale! locale = " + locale); }
			
			return localeSet.dominantBaseline;
		}
		
		
		/** @private */
		static private function addLocale(locale:String):LocaleSettings
		{
			CONFIG::debug{ assert(_localeSettings[locale] == null, "Cannot add a new locale property set when it already exists.  Locale == " + locale); }
			_localeSettings[locale] = new LocaleSettings();
			return _localeSettings[locale];
		}
		
		/** @private */
		static private function initializeDefaultLocales():void
		{
			CONFIG::debug{ assert(_localeSettings == null, "Should not call initializeDefaultLocales when dictionary exists!"); }
			_localeSettings = new Dictionary();
			
			{
				var locale:LocaleSettings = addLocale("en");
				CONFIG::debug{ assert(locale != null, "Failed to create new locale for 'en'!"); }
	
				locale.justificationRule = JustificationRule.SPACE;
				locale.justificationStyle = JustificationStyle.PUSH_IN_KINSOKU;
				locale.leadingModel = LeadingModel.ROMAN_UP;
				locale.dominantBaseline = TextBaseline.ROMAN;
				
				locale = addLocale("ja");
				CONFIG::debug{ assert(locale != null, "Failed to create new locale for 'ja'!"); }
	
				locale.justificationRule = JustificationRule.EAST_ASIAN;
				locale.justificationStyle = JustificationStyle.PUSH_IN_KINSOKU;
				locale.leadingModel = LeadingModel.IDEOGRAPHIC_TOP_DOWN;
				locale.dominantBaseline = TextBaseline.IDEOGRAPHIC_CENTER;
				
				locale = addLocale("zh");
				CONFIG::debug{ assert(locale != null, "Failed to create new locale for 'zh'!"); }
	
				locale.justificationRule = JustificationRule.EAST_ASIAN;
				locale.justificationStyle = JustificationStyle.PUSH_IN_KINSOKU;
				locale.leadingModel = LeadingModel.IDEOGRAPHIC_TOP_DOWN;
				locale.dominantBaseline = TextBaseline.IDEOGRAPHIC_CENTER;
			}		
		}
		
		/** @private */
		static private function getLocale(locale:String):LocaleSettings
		{
			var lowerLocale:String = locale.toLowerCase().substr(0,2);
			var rslt:LocaleSettings = _localeSettings[lowerLocale];
			return rslt == null ? _localeSettings["en"] : rslt;
		}
		
		/** @private */
		static private function fetchLocaleSet(locale:String):LocaleSettings
		{
			if(_localeSettings == null)
				initializeDefaultLocales();
			
			if(locale == _lastLocaleKey)
				return _lastLocale;
			
			var localeSet:LocaleSettings = getLocale(locale);
			
			//update the last locale data
			_lastLocale = localeSet;
			_lastLocaleKey = locale;
			return localeSet;
		}
	}
}

import flashx.textLayout.formats.JustificationRule;
import flash.text.engine.JustificationStyle;
import flashx.textLayout.formats.LeadingModel;
import flash.text.engine.TextBaseline;

import flashx.textLayout.debug.assert;
import flashx.textLayout.tlf_internal;
import flashx.textLayout.formats.TextLayoutFormat;

use namespace tlf_internal;

internal class LocaleSettings
{
	public function LocaleSettings()
	{}
	
	private var _justificationRule:String = null;
	private var _justificationStyle:String = null;
	private var _leadingModel:String = null;
	private var _dominantBaseline:String = null;
	
	public function get justificationRule():String { return _justificationRule; }
	public function set justificationRule(newValue:String):void 
	{ 
		var setValue:Object = TextLayoutFormat.justificationRuleProperty.setHelper(_justificationRule,newValue);
		//don't assert if the newValue is null, only if it is invalid
		CONFIG::debug{ assert(newValue == null || setValue != null, "LocaleUtil should never be providing an invalid JustificationRule!"); }
		
		_justificationRule = setValue == null ? null : (setValue as String);
	}
	
	public function get justificationStyle():String { return _justificationStyle; }
	public function set justificationStyle(newValue:String):void 
	{ 
		var setValue:Object = TextLayoutFormat.justificationStyleProperty.setHelper(_justificationStyle,newValue);
		//don't assert if the newValue is null, only if it is invalid
		CONFIG::debug{ assert(newValue == null || setValue != null, "LocaleUtil should never be providing an invalid LeadingDirection!"); }
		
		_justificationStyle = setValue == null ? null : (setValue as String);
	}
	
	public function get leadingModel():String { return _leadingModel; }
	public function set leadingModel(newValue:String):void 
	{ 
		var setValue:Object = TextLayoutFormat.leadingModelProperty.setHelper(_leadingModel,newValue);
		//don't assert if the newValue is null, only if it is invalid
		CONFIG::debug{ assert(newValue == null || setValue != null, "LocaleUtil should never be providing an invalid JustificationStyle!"); }
		
		_leadingModel = setValue == null ? null : (setValue as String);
	}
	
	
	public function get dominantBaseline():String { return _dominantBaseline; }
	public function set dominantBaseline(newValue:String):void 
	{ 
		var setValue:Object = TextLayoutFormat.dominantBaselineProperty.setHelper(_dominantBaseline,newValue);
		//don't assert if the newValue is null, only if it is invalid
		CONFIG::debug{ assert(newValue == null || setValue != null, "LocaleUtil should never be providing an invalid TextBaseline!"); }
		
		_dominantBaseline = setValue == null ? null : (setValue as String);
	}
}