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
package flashx.textLayout.debug 
{
	[ExcludeClass]
	/** @private */
	public class Debugging 
	{
		import flash.utils.Dictionary;
		import flash.text.engine.TextBlock;
		import flash.text.engine.TextLine;
		import flashx.textLayout.tlf_internal;
		
		use namespace tlf_internal;
		
		CONFIG::debug 
		{
			static tlf_internal var debugOn:Boolean = true;
			static tlf_internal var traceFlag:Boolean = false;	
			static tlf_internal var verbose:Boolean = false;	
			static tlf_internal var throwOnAssert:Boolean = false;
			static tlf_internal var debugCheckTextFlow:Boolean = false;
			static tlf_internal var containerLineValidation:Boolean = false;
			
			static private var traceOutString:String;
			static private var traceChanged:Function = null;
			
			
			static public function traceOut(str:String, ...rest):void
			{
				if (!traceOutString)
					traceOutString = new String();
				for each (var obj:Object in rest)
				{
					str += obj.toString();
					str += " ";
				}

				traceOutString += str + "\n";
				if (traceChanged != null)
					traceChanged(traceOutString);
				else
					trace(str);
			}
			
			static public function setTraceChanged(listener:Function):void
			{
				traceChanged = listener;
			}

			// ///////////////////////////////
			// support creating a log of calls 
			// //////////////////////////////
			
			static tlf_internal var generateDebugTrace:Boolean = false;
			static private var idDictionary:Dictionary = new Dictionary(true);
			static private var nextKey:int = 1;
			static private var callCount:int = 1;
		
			static private const vectPrefix:String = "__AS3__.vec::Vector";
			static private function getClassForIdentity(o:Object):String
			{
				var s:String = flash.utils.getQualifiedClassName(o);
				if (s.substr(0,vectPrefix.length) == vectPrefix)
				{
					s = s.substr( s.lastIndexOf(":")+1);
					return "VectorOf" + s.substr(0,s.length-1);
				}
				return s.substr( s.lastIndexOf(":")+1);
			}
			
			static tlf_internal function getIdentity(o:Object):String
			{
				if (idDictionary[o] == null)
				{
					var s:String = getClassForIdentity(o);
					if (s == "TextLine")
						idDictionary[o] = "textLineArray[" + nextKey + "]"
					else if (s == "TextBlock")
						idDictionary[o] = "textBlockArray[" + nextKey + "]"
					else
						idDictionary[o] = "my" + s + nextKey.toString();
					nextKey++;
				}
				return idDictionary[o];
			}
			
			static tlf_internal function getClassForType(o:Object):String
			{
				var s:String = flash.utils.getQualifiedClassName(o);
				if (s.substr(0,vectPrefix.length) == vectPrefix)
				{
					s = s.substr( s.lastIndexOf(":")+1);
					return "Vector.<" + s ;
				}
				return s.substr( s.lastIndexOf(":")+1);
			}
			
			static tlf_internal function printHexString(tempString:String):String
			{
				var str:String = "String.fromCharCode("
				for (var idx:int = 0; idx < tempString.length; idx++)
				{
					if (idx != 0)
						str += ", ";
					str += "0x" + tempString.charCodeAt(idx).toString(16);
				}
				str += ")";
				return str;
			}
			
			static tlf_internal function makeParameter(obj:Object):String
			{
				var str:String;
				if (obj == null)
					str = "null"
				else if (obj is String)
				{
					var tempString:String = String(obj);
					var idx:int;
					
					// any out of bounds characters?
					var allokay:Boolean = true;
					for (idx = 0; idx < tempString.length; idx++)
					{
						if (tempString.charCodeAt(idx) > 0xFF)
						{
							allokay = false;
							break;
						}
					}
					if (allokay)
						str = "\"" + tempString +  "\"";
					else
					{
						str = "String.fromCharCode("
						for (idx = 0; idx < tempString.length; idx++)
						{
							if (idx != 0)
								str += ", ";
							str += "0x" + tempString.charCodeAt(idx).toString(16);
						}
						str += ")";
					}
				}
				else if (obj is Number || obj is int)
					str = obj.toString();
				else if (obj is Boolean)
					str = Boolean(obj) ? "true" : "false";
				else 
					str = getIdentity(obj);
				return str;
			}
			
			/** @private Trace a function call */
			static tlf_internal function traceFTECall(rslt:Object,target:Object,method:String,...rest):void
			{
				if (!generateDebugTrace)
					return;
					
				// if result already exists then null it out - its a side effect
				if (idDictionary[rslt] != null)
					rslt = null;
				var str:String = "	";
				var rsltType:String;
				if (rslt)
				{
					rsltType = getClassForType(rslt);
					if (rslt is TextLine)
						str += getIdentity(rslt) + " = ";
					else if (rslt is TextBlock)
						str += getIdentity(rslt) + " = ";
					else
						str += "var " + getIdentity(rslt) + ":" + getClassForType(rslt) + " = ";
				}
				
				if (target)
					str += getIdentity(target) + ".";
				
				if (rest.length == 0)
				{
					str += method + ";";
				}
				else
				{
					str += method + "(";
					for (var i:int =0; i<rest.length; i++)
					{
						if (i != 0)
							str += ","
						str += makeParameter(rest[i]);
					}
	
					if (rslt)
						str += ") as " + rsltType + ";"; 
					else
						str += ");"
				}
				str += " // " + callCount.toString();
				trace(str);
				callCount++;
			}
			
			/** @private Trace a property assignment */
			static tlf_internal function traceFTEAssign(target:Object,prop:String,val:Object):void
			{
				if (!generateDebugTrace)
					return;
					
				var str:String = "	" + getIdentity(target) + "." + prop + " = " + makeParameter(val) + ";" +  "// " + callCount.toString();
				trace(str);
				callCount++;
			}
		}
	}
} // end package
