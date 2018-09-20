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
package flashx.textLayout.factory
{
	import flashx.textLayout.formats.ITextLayoutFormat;
	
	/** 
	 * The TruncationOptions class specifies options for limiting the number of lines of text 
	 * created by a text line factory and for indicating when lines have been left out.
	 * 
	 * @playerversion Flash 10
	 * @playerversion AIR 1.5
	 * @langversion 3.0
	 */
	public final class TruncationOptions
	{
		/** 
		 * Creates a TruncationOptions object.
		 * 
		 * @param truncationIndicator the string used to indicate that text has been truncated. 
		 * It appears at the end of the composed text. The default value is the horizontal ellipsis (U+2026).
		 * @param lineCountLimit specifies a truncation criterion in the form of the maximum 
		 * number of lines allowed. The default value of <code>NO_LINE_COUNT_LIMIT</code> 
		 * indicates that there is no line count limit.
		 * @param truncationIndicatorFormat specifies the format for the truncation indicator. 
		 * A null format (the default value) specifies that the truncation indicator assume 
		 * the format of content just before the truncation point. The <code>TextLineFactory</code> 
		 * methods that take a simple string as input also ignore this parameter and implement 
		 * the default behavior.
		 * @playerversion Flash 10
		 * @playerversion AIR 1.5
		 * @langversion 3.0
		 */
		public function TruncationOptions (truncationIndicator:String=HORIZONTAL_ELLIPSIS, lineCountLimit:int=NO_LINE_COUNT_LIMIT, truncationIndicatorFormat:ITextLayoutFormat=null)
		{
			this.truncationIndicator =  truncationIndicator;
			this.truncationIndicatorFormat = truncationIndicatorFormat;
			this.lineCountLimit = lineCountLimit;
		}
		
		/** 
		 * A string used to indicate that content could not be fully displayed
		 * because of limits on the number of lines.
		 * 
		 * @return the truncation indicator
		 * @playerversion Flash 10
		 * @playerversion AIR 1.5
		 * @langversion 3.0
		 */
		public function get truncationIndicator():String
		{
			return _truncationIndicator ? _truncationIndicator : HORIZONTAL_ELLIPSIS;
		}
		/** 
		 * Sets the truncation indicator
		 * @param val the string used to indicate that text has been truncated
		 * @playerversion Flash 10
		 * @playerversion AIR 1.5
		 * @langversion 3.0
		 */
		public function set truncationIndicator(val:String):void
		{
			_truncationIndicator = val;
		}
		
		/** 
		 * The style applied to the truncation indicator string.
		 * @return the format truncation indicator
		 * @playerversion Flash 10
		 * @playerversion AIR 1.5
		 * @langversion 3.0
		 */
		public function get truncationIndicatorFormat():ITextLayoutFormat
		{
			return _truncationIndicatorFormat;
		}
		/** 
		 * Sets the styles applied to the truncation indicator character
		 * 
		 * @param val specifies the format for the truncation indicator. A null format specifies that the truncation indicator assume the format of content
		 * just before the truncation point. The <code>TextLineFactory</code> methods that take a simple string as input also ignore this parameter and implement the default behavior
		 * @playerversion Flash 10
		 * @playerversion AIR 1.5
		 * @langversion 3.0
		 */
		public function set truncationIndicatorFormat(val:ITextLayoutFormat):void
		{
			_truncationIndicatorFormat = val;
		}
		
		/** 
		 * The maximum number of lines to create.
		 * @return the line count limit
		 * @playerversion Flash 10
		 * @playerversion AIR 1.5
		 * @langversion 3.0
		 */
		public function get lineCountLimit():int
		{
			return _lineCountLimit < NO_LINE_COUNT_LIMIT ? 0 : _lineCountLimit;
		}
		/** 
		 * Sets the maximum number of lines to create
		 * @param val specifies the maximum number of lines allowed. 
		 * A value of <code>NO_LINE_COUNT_LIMIT</code> indicates that there is no line count limit.
		 */
		public function set lineCountLimit(val:int):void
		{
			_lineCountLimit = val;
		} 
		
		private var _truncationIndicator:String;
		private var _truncationIndicatorFormat:ITextLayoutFormat;
		private var _lineCountLimit:int;
		
		/**
		 * Defines the <code>lineCountLimit</code> property value, <code>-1</code>, that represents no limit.
		 *
		 * @see #lineCountLimit
		 * @playerversion Flash 10
		 * @playerversion AIR 1.5
		 * @langversion 3.0
		 */
		public static const NO_LINE_COUNT_LIMIT:int = -1;

		/**
		 * Defines the <code>truncationIndicator</code> property value, <code>\u2026</code>, that represents a horizontal ellipsis.
		 *
		 * @see #truncationIndicator
		 * @playerversion Flash 10
		 * @playerversion AIR 1.5
		 * @langversion 3.0
		 */
		public static const HORIZONTAL_ELLIPSIS:String = "\u2026";
	}	
}


