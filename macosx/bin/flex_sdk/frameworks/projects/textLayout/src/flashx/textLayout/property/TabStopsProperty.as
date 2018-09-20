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
package flashx.textLayout.property
{
	import flash.text.engine.TabAlignment;
	
	import flashx.textLayout.debug.assert;
	import flashx.textLayout.formats.FormatValue;
	import flashx.textLayout.formats.TabStopFormat;
		
	[ExcludeClass]
	/** Property for tab stops. Extends ArrayProperty; setter takes a string representation of tab stops in addition to an array. @private */
	public class TabStopsProperty extends ArrayProperty
	{
		public function TabStopsProperty(nameValue:String, defaultValue:Array, inherited:Boolean, categories:Vector.<String>)
		{ 
			super(nameValue, defaultValue, inherited, categories, TabStopFormat);
		}
			
		/** Helper function when setting the property */
		public override function setHelper(currVal:*,newVal:*):*
		{
			// null is a real value - DO NOT map to undefined like the others
			
			// null, undefined and INHERIT are all valid for arrays
			if (newVal == null || newVal == FormatValue.INHERIT)
				return newVal;
				
			// Accepts either an array or a string representation
			var tabStops:Array = newVal as Array;
			if (tabStops)
			{
				if (!checkArrayTypes(tabStops))
				{
					Property.errorHandler(this,newVal);
					return currVal;
				}
			}
			else
			{
				var valString:String = newVal as String;
				if (!valString)
				{
					Property.errorHandler(this,newVal);
					return currVal;
				}
					
				// Parse the string representation and create an equivalent array
				tabStops = new Array();
				
				// Replace escape sequences (\ followed by a space or \) with placeholder strings
				// that can't naturally occur in the passed-in string 
				valString = valString.replace(_escapeBackslashRegex, _backslashPlaceHolder);
				valString = valString.replace(_escapeSpaceRegex, _spacePlaceHolder);
				
				_tabStopRegex.lastIndex = 0;
				do
				{
					var result:Object = _tabStopRegex.exec(valString);
					if (!result)
						break; // no more matches
						
					var tabStop:TabStopFormat = new TabStopFormat();
					switch (result[1].toLowerCase())
					{
						case "s":
						case "": // START is the default
							tabStop.alignment = TabAlignment.START;
							break;
						case "c":
							tabStop.alignment = TabAlignment.CENTER;
							break;
						case "e":
							tabStop.alignment = TabAlignment.END;
							break;
						case "d":
							tabStop.alignment = TabAlignment.DECIMAL;
							break;
					}
					
					var position:Number = Number(result[2]); 
					if (isNaN(position))
					{
						Property.errorHandler(this,newVal);
						return currVal;
					}
					tabStop.position = position;

					if (tabStop.alignment == TabAlignment.DECIMAL)
					{
						if (result[3] == "")
							tabStop.decimalAlignmentToken = "."; //default
						else
						{
							// strip the leading vertical bar and restore \ and space characters where intended
							tabStop.decimalAlignmentToken = result[3].slice(1).replace(_backslashPlaceholderRegex, "\\");
							tabStop.decimalAlignmentToken = tabStop.decimalAlignmentToken.replace(_spacePlaceholderRegex, " ");
						}
					}
					else if (result[3] != "")
					{
						Property.errorHandler(this,newVal);
						return currVal; // if alignment is not decimal, the alignment token is not allowed
					}
						
					tabStops.push(tabStop);
				
				} while (true);
			
			}

			return tabStops.sort(compareTabStopFormats);
		}
		
		/** @private */
		public override function toXMLString(val:Object):String
		{
			var str:String = "";
			var tabStops:Array = val as Array;
			for each (var tabStop:TabStopFormat in tabStops)
			{
				if (str.length)
					str += " ";
				
				switch (tabStop.alignment)
				{
					case TabAlignment.START:
						str += "s";
						break;
					case TabAlignment.CENTER:
						str += "c";
						break;
					case TabAlignment.END:
						str += "e";
						break;
					case TabAlignment.DECIMAL:
						str += "d";
						break;
				}
				
				str += tabStop.position.toString();
				
				if (tabStop.alignment == TabAlignment.DECIMAL)
				{
					var escapedAlignmentToken:String = tabStop.decimalAlignmentToken.replace(_backslashRegex, "\\\\");
					escapedAlignmentToken = escapedAlignmentToken.replace(_spaceRegex, "\\ ");
					str += "|" + escapedAlignmentToken;
				}
			}
			
			return str;
		}
		
		private static function compareTabStopFormats(a:TabStopFormat, b:TabStopFormat):Number
		{
			return a.position == b.position ? 0 : a.position < b.position ? -1 : 1;
		}
		
		// Alignment type: [sScCeEdD]?  - Atmost 1 occurance of one of s/c/e/d (or upper-case equivalents)
		// Position: [^| ]+ - At least character which is not a space or | (further validation is done by the Number constructor)
		// Alignment token and separator:(|[^ ]*)? - Atmost one occurance of a | followed by 0 or more non-space characters
		// Delimiter: ( |$) - A space or end-of-string
		private static const _tabStopRegex:RegExp = /([sScCeEdD]?)([^| ]+)(|[^ ]*)?( |$)/g;
		private static const _escapeBackslashRegex:RegExp = /\\\\/g;
		private static const _escapeSpaceRegex:RegExp = /\\ /g;
		private static const _backslashRegex:RegExp = /\\/g;
		private static const _spaceRegex:RegExp = / /g;
		private static const _backslashPlaceholderRegex:RegExp = /\uE000/g;
		private static const _spacePlaceholderRegex:RegExp = /\uE001/g;
		
		// Replace escape sequences (\ followed by a space or \) with placeholder strings
		// containing characters from Unicode private use area (won't naturally occur in the passed-in string) 
		private static const _backslashPlaceHolder:String = String.fromCharCode(0xE000);
		private static const _spacePlaceHolder:String = String.fromCharCode(0xE001);
				
	}
}
