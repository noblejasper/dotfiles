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
package flashx.textLayout.operations
{
	import flash.utils.getQualifiedClassName;
	
	import flashx.textLayout.edit.ParaEdit;
	import flashx.textLayout.edit.SelectionState;
	import flashx.textLayout.elements.FlowLeafElement;
	import flashx.textLayout.elements.ParagraphElement;
	import flashx.textLayout.elements.SpanElement;
	import flashx.textLayout.elements.TextFlow;
	import flashx.textLayout.formats.ITextLayoutFormat;
	import flashx.textLayout.formats.TextLayoutFormat;
	import flashx.textLayout.tlf_internal;


	use namespace tlf_internal;


	/**
	 * The SplitParagraphOperation class encapsulates a change that splits a paragraph into two elements.
	 *
	 * <p>The operation creates a new paragraph containing the text from 
	 * the specified position to the end of the paragraph. If a range of text is specified, the text 
	 * in the range is deleted first.</p>
	 * 
	 * @see flashx.textLayout.elements.ParagraphElement
	 * @see flashx.textLayout.edit.EditManager
	 * @see flashx.textLayout.events.FlowOperationEvent
	 * 
	 * @playerversion Flash 10
	 * @playerversion AIR 1.5
	 * @langversion 3.0 
	 */			
	public class SplitParagraphOperation extends SplitElementOperation
	{		
		/** 
		 * Creates a SplitParagraphOperation object.
		 * 
		 * @param operationState Describes the point at which to split the paragraph.
		 * If a range of text is specified, the contents of the range are deleted.
		 * 
		 * @playerversion Flash 10
		 * @playerversion AIR 1.5
	 	 * @langversion 3.0 
		 */
		public function SplitParagraphOperation(operationState:SelectionState)
		{
			var para:ParagraphElement = operationState.textFlow.findLeaf(operationState.absoluteStart).getParagraph();
			super(operationState, para);
		}
		
		/** @private */
		tlf_internal override function merge(operation:FlowOperation):FlowOperation
		{
			if (this.endGeneration != operation.beginGeneration)
				return null;
			// TODO we could probably do something a bit more efficient for a backspace
			if ((operation is SplitParagraphOperation) || (operation is InsertTextOperation))
				return new CompositeOperation([this,operation]);
			return null;
		}
	}
}