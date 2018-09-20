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
package flashx.textLayout.conversion
{
	//import container.TextFrame;
	import flash.display.DisplayObjectContainer;
	
	import flashx.textLayout.elements.DivElement;
	import flashx.textLayout.elements.FlowElement;
	import flashx.textLayout.elements.FlowGroupElement;
	import flashx.textLayout.elements.IConfiguration;
	import flashx.textLayout.elements.ParagraphElement;
	import flashx.textLayout.elements.FlowLeafElement;
	import flashx.textLayout.elements.SpanElement;
	import flashx.textLayout.elements.TextFlow;
	import flashx.textLayout.tlf_internal;
	
	use namespace tlf_internal;

	[ExcludeClass]
	/** 
	 * @private
	 * PlainText import converter. Use this to import simple unformatted Unicode text.
	 * Newlines will be converted to paragraphs. Using the PlainTextImporter directly
	 * is equivalent to calling TextConverter.importToFlow(TextConverter.PLAIN_TEXT_FORMAT).
	 */
	internal class PlainTextImporter extends ConverterBase implements ITextImporter
	{
		protected var _config:IConfiguration = null;
		
		/** Constructor */
		public function PlainTextImporter()
		{
		}
		
		/** @copy ITextImporter#importToFlow()
		 */
		public function importToFlow(source:Object):TextFlow
		{
			if (source is String)
				return importFromString(String(source));
			return null;
		}
		
		/**
		 * The <code>configuration</code> property contains the IConfiguration instance that
		 * the importerd needs when creating new TextFlow instances. This property
		 * is initially set to <code>null</code>.
		 * @see TextFlow constructor
		 * @playerversion Flash 10.2
		 * @playerversion AIR 2.0
		 * @langversion 3.0
		 */
		public function get configuration():IConfiguration
		{
			return _config;
		}
		
		public function set configuration(value:IConfiguration):void
		{
			_config = value;
		}
		
		
		// LF or CR or CR+LF. Equivalently, LF or CR, the latter optionally followed by LF
		private static const _newLineRegex:RegExp = /\u000A|\u000D\u000A?/g;
		
		/** Import text content, from an external source, and convert it into a TextFlow.
		 * @param source		source data to convert
		 * @return textFlows[]	an array of TextFlow objects that were created from the source.
		 */
		protected function importFromString(source:String):TextFlow
		{
			var paragraphStrings:Array = source.split(_newLineRegex);

			var textFlow:TextFlow = new TextFlow(_config);
			var paraText:String;
			for each (paraText in paragraphStrings)
			{
				var paragraph:ParagraphElement  = new ParagraphElement();	// No PMD
				var span:SpanElement = new SpanElement();	// No PMD
				span.replaceText(0, 0, paraText);
				paragraph.replaceChildren(0, 0, span);			
				textFlow.replaceChildren(textFlow.numChildren, textFlow.numChildren, paragraph);
			}

			// Mark partial last paragraph (string doesn't end in paragraph terminator)
			if (useClipboardAnnotations && 
				(source.lastIndexOf('\u000A', source.length - 2) < 0 || 
					source.lastIndexOf('\u000D\u000A', source.length - 3) < 0))
			{
				var lastLeaf:FlowLeafElement = textFlow.getLastLeaf();
				lastLeaf.setStyle(ConverterBase.MERGE_TO_NEXT_ON_PASTE, "true");
				lastLeaf.parent.setStyle(ConverterBase.MERGE_TO_NEXT_ON_PASTE, "true");
				textFlow.setStyle(ConverterBase.MERGE_TO_NEXT_ON_PASTE, "true");
			}
			return textFlow;			
		}

	}
}
