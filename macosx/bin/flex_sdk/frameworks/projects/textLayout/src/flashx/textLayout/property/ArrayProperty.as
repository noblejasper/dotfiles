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
	import flashx.textLayout.formats.FormatValue;
	import flashx.textLayout.tlf_internal;
		
	use namespace tlf_internal;
		
	[ExcludeClass]
	/** A property description with an Array value.@private */
	public class ArrayProperty extends Property
	{
		private var _memberType:Class;
		
		public function ArrayProperty(nameValue:String, defaultValue:Array, inherited:Boolean, categories:Vector.<String>, mType:Class)
		{
			super(nameValue, defaultValue, inherited, categories); 
			_memberType = mType;
			CONFIG::debug { assert(_memberType.description != null,"Array member class must have description"); }
			// can defaultValue be INHERIT?
			CONFIG::debug { assert(checkArrayTypes(defaultValue),"Array has bad defaultValue"); }
		}
		
		/** The type the members of the array are required to be. */
		public function get memberType():Class
		{ return _memberType; }
		
		protected function checkArrayTypes(val:Object):Boolean
		{
			if (val == null)
				return true;
			if (!(val is Array))
				return false;
			if (_memberType == null)
				return true;
			for each (var obj:Object in (val as Array))
			{
				if (!(obj is _memberType))
					return false
			}
			return true;
		}
		
		/** @private */
		public override function get defaultValue():*
		{ return super.defaultValue == null ? null : (super.defaultValue as Array).slice();	}
				
		/** @private */
		public override function setHelper(currVal:*,newVal:*):*
		{
			if (newVal === null)
				newVal = undefined;
			
			if (newVal == undefined || newVal == FormatValue.INHERIT)
				return newVal;

			if (newVal is String)
				newVal = this.valueFromString(String(newVal));
			
			if (!checkArrayTypes(newVal))
			{
				Property.errorHandler(this,newVal);
				return currVal;
			}
			return (newVal as Array).slice(); 
		}

		/** @private */
		public override function concatInheritOnlyHelper(currVal:*,concatVal:*):*
		{
			return (inherited && currVal === undefined) || currVal == FormatValue.INHERIT ? ((concatVal is Array) ? (concatVal as Array).slice() : concatVal) : currVal;

		}	
		/** @private */
		public override function concatHelper(currVal:*,concatVal:*):*
		{
			if (inherited)
				return currVal === undefined || currVal == FormatValue.INHERIT ? ((concatVal is Array) ? (concatVal as Array).slice() : concatVal) : currVal;
			if (currVal === undefined)
				return defaultValue;
			return currVal == FormatValue.INHERIT ? ((concatVal is Array) ? (concatVal as Array).slice() : concatVal) : currVal;

		}	
		/** @private */
		public override function equalHelper(v1:*,v2:*):Boolean
		{
			if (_memberType != null)
			{			
				var v1Array:Array = v1 as Array;
				var v2Array:Array = v2 as Array;
		
				if (v1Array && v2Array)
				{			
					if (v1Array.length == v2Array.length)
					{
						var desc:Object = _memberType.description;
						for (var i:int=0; i < v1Array.length; ++i)
						{
							if (!Property.equalAllHelper(desc, v1[i], v2[i]))
								return false;
						}
						return true;
					}
				}
			}
			return v1 == v2;				
		}
		
		/** @private */
		public override function toXMLString(val:Object):String
		{
			if (val == FormatValue.INHERIT)
				return String(val);
			// TODO-7/7/2008-The XML format for array properties (as implemented below)
			// is appropriate for what it is currently used, but can be ambiguous.
			// For example, what if XML representations of contained elements contain the delimiters used here? 
			 
			// TODO: Check for description?
			var desc:Object = _memberType.description;
			var rslt:String = "";
			var addSemi:Boolean = false;
			for each (var member:Object in val)
			{
				if (addSemi)
					rslt += "; "
				// export each element ',' separated
				var addComma:Boolean = false;
				for each (var prop:Property in desc)
				{
					var val:Object = member[prop.name];
					if (val != null)
					{
						if (addComma)
							rslt += ", ";
						rslt += prop.name + ":" + prop.toXMLString(val);
						addComma = true;
					}
				}
				addSemi = true;
			}
			return rslt;
		}
		
		/** @private */
		private function valueFromString(str:String):*
		{ 	
			// TODO-7/7/2008-The XML format for array properties can be ambiguous.
			// See comment in toXMLString.
			if ((str == null) || (str == "")) 
				return null;
			if (str == FormatValue.INHERIT)
				return str;
			var result:Array = new Array();
			var desc:Object = _memberType.description;
			
			var attrsAll:Array = str.split('; '); 
			for each (var attrs:String in attrsAll)
			{
			 	var obj:Object = new _memberType();	// NO PMD
			 	
			 	var attrsOne:Array = attrs.split(', ');
			 	for each (var attr:String in attrsOne)
			 	{
			 		var nameValArr:Array = attr.split(':');
			 		var propName:String = nameValArr[0];
			 		var propVal:String = nameValArr[1];
			 	
				 	for each (var prop:Property in desc)
				 	{
				 		if (prop.name == propName)
				 		{
				 			obj[propName] = prop.setHelper(propVal,obj[propName]);
				 			break;
				 		}
			 		}
			 	}
			 	result.push(obj);
			}
				
			return result; 
		}
	}
}
