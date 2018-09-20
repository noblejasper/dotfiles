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
	import flashx.textLayout.debug.assert;
	import flashx.textLayout.elements.ListElement;
	import flashx.textLayout.tlf_internal;
	
	use namespace tlf_internal;
	
	[ExcludeClass]
	public class SpacingLimitPropertyHandler extends PropertyHandler
	{
		private var _minPercentValue:String;
		private var _maxPercentValue:String;
		
		public function SpacingLimitPropertyHandler(minPercentValue:String, maxPercentValue:String)
		{ 
			_minPercentValue = minPercentValue; 
			_maxPercentValue = maxPercentValue; 
		}
		
		// check to see if this handler has a custom exporter that must be used when owningHandlerCheck is true
		public override function get customXMLStringHandler():Boolean
		{ return true; }
		
		public override function toXMLString(val:Object):String
		{ 
			if (val.hasOwnProperty("optimumSpacing") && val.hasOwnProperty("minimumSpacing") && val.hasOwnProperty("maximumSpacing"))
				return val.optimumSpacing.toString() + ',' + val.minimumSpacing.toString() + ',' + val.maximumSpacing.toString();
			return val.toString();
		}
		
		/** matches a single spacing limit value */
		static private  const _spacingLimitPattern:RegExp = /\d+%/g;
		/** matches an array of 1-3 spacing limit values, separated by commas */
		static private  const _spacingLimitArrayPattern:RegExp = /^\s*(\d+%)(\s*,\s*)(\d+%)?(\s*,\s*)(\d+%)?\s*$/;
		
		// return a value if this handler "owns" this property - otherwise return undefined
		public override function owningHandlerCheck(newVal:*):*
		{ 
			if (newVal is String)
			{
				if (_spacingLimitArrayPattern.test(newVal))
					return newVal;
			}
			
			else if (newVal.hasOwnProperty("optimumSpacing") && newVal.hasOwnProperty("minimumSpacing") && newVal.hasOwnProperty("maximumSpacing"))
				return newVal;

			return undefined;
		}
		
		private function checkValue(value:*):Boolean
		{
			var minLegalValue:Number = Property.toNumberIfPercent(_minPercentValue);
			var maxLegalValue:Number = Property.toNumberIfPercent(_maxPercentValue);
			
			// Check that the 3 values don't exceed the minimum or maximum percentages allowed
			var optValue:Number = Property.toNumberIfPercent(value.optimumSpacing);
			if (optValue < minLegalValue || optValue > maxLegalValue)
				return false;
			
			var minValue:Number = Property.toNumberIfPercent(value.minimumSpacing);
			if (minValue < minLegalValue || minValue > maxLegalValue)
				return false;

			var maxValue:Number = Property.toNumberIfPercent(value.maximumSpacing);
			if (maxValue < minLegalValue || maxValue > maxLegalValue)
				return false;
			
			// Check that optimum is between minimum & maximum
			if (optValue < minValue || optValue > maxValue)
				return false;
			
			// Check that minimum is below maximum
			if (minValue > maxValue)
				return false;
			
			return true;
		}
		
		/** parse the input string and create a valid input value */
		public override function setHelper(newVal:*):*
		{ 
			var s:String = newVal as String;
			
			if (s == null)
				return newVal;	// assume its an object that's been parsed already
			
			if (_spacingLimitArrayPattern.test(newVal))
			{
				// Incoming string is comma-delimited list of 1-3 percentage values. 
				// If the incoming string specifies just one value, its the optimumSpacing, and minimumSpacing and maximumSpacing get the default values.
				// If the incoming string specifies two values, the first one is optimumSpacing and minimumSpacing and the second is maximumSpacing
				// If the incoming string specifies three values, the first one is optimumSpacing and the second is minimumSpacing and the third is maximumSpacing
				var result:Object = new Object();
				var splits:Array = s.match(_spacingLimitPattern);
				if (splits.length  == 1)
				{
					result.optimumSpacing = splits[0];
					result.minimumSpacing = result.optimumSpacing;
					result.maximumSpacing = result.optimumSpacing;
				}
				else if (splits.length == 3)
				{
					result.optimumSpacing = splits[0];
					result.minimumSpacing = splits[1];
					result.maximumSpacing = splits[2];
				}
				else 
					return undefined;
				if (checkValue(result))
					return result;
			}

			return undefined;
		}
	}
}
