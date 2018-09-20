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
	import flashx.textLayout.debug.assert;
	import flashx.textLayout.edit.ModelEdit;
	import flashx.textLayout.edit.ParaEdit;
	import flashx.textLayout.edit.PointFormat;
	import flashx.textLayout.edit.SelectionState;
	import flashx.textLayout.edit.TextFlowEdit;
	import flashx.textLayout.elements.FlowElement;
	import flashx.textLayout.elements.FlowLeafElement;
	import flashx.textLayout.elements.LinkElement;
	import flashx.textLayout.elements.SpanElement;
	import flashx.textLayout.elements.TCYElement;
	import flashx.textLayout.formats.ITextLayoutFormat;
	import flashx.textLayout.formats.TextLayoutFormat;
	import flashx.textLayout.tlf_internal;

	use namespace tlf_internal;


	/**
	 * The InsertTextOperation class encapsulates a text insertion operation.
	 *
	 * @see flashx.textLayout.edit.EditManager
	 * @see flashx.textLayout.events.FlowOperationEvent
	 * 
	 * @playerversion Flash 10
	 * @playerversion AIR 1.5
	 * @langversion 3.0 
	 */
	public class InsertTextOperation extends FlowTextOperation
	{
		private var _deleteSelectionState:SelectionState;
		private var delSelOp:DeleteTextOperation = null; 
		/** @private - this should be private but too late for code changes on Labs */
		public var _text:String;
		
		private var _pointFormat:ITextLayoutFormat;
			
		/** 
		 * Creates an InsertTextOperation object.
		 * 
		 * @param operationState Describes the insertion point or range of text.
		 * @param text The string to insert.
		 * @param deleteSelectionState Describes the range of text to delete before doing insertion, 
		 * if different than the range described by <code>operationState</code>.
		 * 
		 * @playerversion Flash 10
		 * @playerversion AIR 1.5
	 	 * @langversion 3.0 
		 */	
		public function InsertTextOperation(operationState:SelectionState, text:String, deleteSelectionState:SelectionState = null)
		{
			super(operationState);
			
			_pointFormat = operationState.pointFormat;
			_text = text;
			
			initialize(deleteSelectionState);
		}
		
		private function initialize(deleteSelectionState:SelectionState):void
		{	
			if (deleteSelectionState == null)
				deleteSelectionState = originalSelectionState;
			if (deleteSelectionState.anchorPosition != deleteSelectionState.activePosition)
			{
				_deleteSelectionState = deleteSelectionState;
				delSelOp = new DeleteTextOperation(_deleteSelectionState);
			}
		}
		
		/** 
		 * The text inserted by this operation. 
		 * @playerversion Flash 10
		 * @playerversion AIR 1.5
	 	 * @langversion 3.0 
		*/
		public function get text():String
		{
			return _text;
		}
		public function set text(value:String):void
		{
			_text = value;
		}
		
		/** 
		 * The text deleted by this operation, if any.
		 * 
		 * <p><code>null</code> if no text is deleted.</p>
		 * 
		 * @playerversion Flash 10
		 * @playerversion AIR 1.5
	 	 * @langversion 3.0 
		*/
		public function get deleteSelectionState():SelectionState
		{
			return _deleteSelectionState;
		}
		public function set deleteSelectionState(value:SelectionState):void
		{
			_deleteSelectionState = value;
		}
		
		/** 
		 * The character format applied to the inserted text.
		 *  
		 * @playerversion Flash 10
		 * @playerversion AIR 1.5
	 	 * @langversion 3.0 
		*/
		public function get characterFormat():ITextLayoutFormat
		{
			return _pointFormat;
		}
		public function set characterFormat(value:ITextLayoutFormat):void
		{
			_pointFormat = new PointFormat(value);
		}
		
		private function doDelete(leaf:FlowLeafElement):ITextLayoutFormat
		{			
			// User selected a range of text and is replacing it. We're doing the delete here.
			// We preserve the format from the deleted text, and apply it to the text insert,
			// unless the user has specified an alternate format.
			var deleteFormat:PointFormat = PointFormat.createFromFlowElement(textFlow.findLeaf(absoluteStart));
			var beforeDeleteFormat:PointFormat = absoluteStart == leaf.getParagraph().getAbsoluteStart() ? null : PointFormat.createFromFlowElement(textFlow.findLeaf(absoluteStart - 1));

			if (delSelOp.doOperation())		// figure out what to do here
			{
				//do not change characterFormat if user specified one already, or if its the same as in the surrounding text.
				// If the surrounding text is the same, forcing the point format requires more composition because it inserts
				// the text in its own span.
				if (!_pointFormat && (absoluteStart < absoluteEnd) && PointFormat.isEqual(deleteFormat, beforeDeleteFormat))
					deleteFormat = null;
				else 
				{
					// If the leaf element is empty, remove it now
					if (leaf.textLength == 0) 
						leaf.parent.removeChild(leaf);
				}
			} 
			return deleteFormat;
		}
				
		private function applyPointFormat(span:SpanElement, pointFormat:ITextLayoutFormat):void
		{
			if (!TextLayoutFormat.isEqual(pointFormat, span.format))
			{
				var spanFormat:TextLayoutFormat = new TextLayoutFormat(span.format);
				spanFormat.apply(pointFormat);
				span.format = spanFormat;
			}
			if (pointFormat is PointFormat)
			{
				var pf:PointFormat = pointFormat as PointFormat;
				if (pf.linkElement)
				{
					if (pf.linkElement.href)
					{
						TextFlowEdit.makeLink(textFlow, absoluteStart, absoluteStart + _text.length, pf.linkElement.href, pf.linkElement.target);
						var linkLeaf:FlowLeafElement = textFlow.findLeaf(absoluteStart);
						var linkElement:FlowElement = linkLeaf.getParentByType(LinkElement);
						linkElement.format = pf.linkElement.format;
					}
				}
				if (pf.tcyElement)
				{
						TextFlowEdit.makeTCY(textFlow, absoluteStart, absoluteStart + _text.length);
						var tcyLeaf:FlowLeafElement = textFlow.findLeaf(absoluteStart);
						var tcyElement:FlowElement = tcyLeaf.getParentByType(TCYElement);
						tcyElement.format = pf.tcyElement.format;
				}
				else if (span.getParentByType(TCYElement))
					TextFlowEdit.removeTCY(textFlow, absoluteStart, absoluteStart + _text.length);
			}
		}
		private function doInternal():void
		{
			var deleteFormat:ITextLayoutFormat;
			
			if (delSelOp != null) 
				deleteFormat = doDelete(textFlow.findLeaf(absoluteStart));
						
			var span:SpanElement = ParaEdit.insertText(textFlow, absoluteStart, _text, _pointFormat != null || deleteFormat != null/* createNewSpan */);
			if (textFlow.interactionManager)
				textFlow.interactionManager.notifyInsertOrDelete(absoluteStart, _text.length);
			
			if (span != null)
			{
				if (deleteFormat)
				{
					span.format = deleteFormat;
					applyPointFormat(span, deleteFormat);
					if ((deleteFormat is PointFormat) && PointFormat(deleteFormat).linkElement && PointFormat(deleteFormat).linkElement.href && originalSelectionState.selectionManagerOperationState && textFlow.interactionManager)
					{
						// set pointFormat from leafFormat, to insure link attributes are propagated from replaced text to next insertion
						// if I select a range of text in a link, and type over it to replace, the new text should be in a link with the same settings.
						var state:SelectionState = textFlow.interactionManager.getSelectionState();
						state.pointFormat = PointFormat.clone(deleteFormat as PointFormat);
						textFlow.interactionManager.setSelectionState(state);
					}
				}
				if (_pointFormat)
					applyPointFormat(span, _pointFormat);
			}
		}
		
		/** @private */
		public override function doOperation():Boolean
		{
			doInternal();
			return true;
		}
	
		/** @private */
		public override function undo():SelectionState
		{ 
			ModelEdit.deleteText(textFlow, absoluteStart, absoluteStart + _text.length, false);
			
			var newSelectionState:SelectionState = originalSelectionState;
			if (delSelOp != null)
				newSelectionState = delSelOp.undo();
			
			return originalSelectionState;
		}
		
		/**
		 * Re-executes the operation after it has been undone.
		 * 
		 * <p>This function is called by the edit manager, when necessary.</p>
		 * 
		 * @playerversion Flash 10
		 * @playerversion AIR 1.5
	 	 * @langversion 3.0 
		 */
		public override function redo():SelectionState
		{ 
			doInternal();
			return new SelectionState(textFlow,absoluteStart+_text.length,absoluteStart+_text.length,null);
		}

		/** @private */
		tlf_internal override function merge(op2:FlowOperation):FlowOperation
		{
			if (absoluteStart < absoluteEnd)
				return null;
			if (this.endGeneration != op2.beginGeneration)
				return null;
			// We are assuming here that these operations are contiguous, because
			// SelectionManager doesn't try to merge operations if the selection
			// has changed
			var insertOp:InsertTextOperation = null;
			if (op2 is InsertTextOperation)
				insertOp = op2 as InsertTextOperation;
			if (insertOp)
			{
				if (insertOp.deleteSelectionState != null || deleteSelectionState != null)
					return null;
				if ((insertOp.originalSelectionState.pointFormat == null) && (originalSelectionState.pointFormat != null))
					return null;
				if ((originalSelectionState.pointFormat == null) && (insertOp.originalSelectionState.pointFormat != null))
					return null;
				if (originalSelectionState.absoluteStart + _text.length != insertOp.originalSelectionState.absoluteStart)
					return null;
				if (((originalSelectionState.pointFormat == null) && (insertOp.originalSelectionState.pointFormat == null)) ||
					(PointFormat.isEqual(originalSelectionState.pointFormat, insertOp.originalSelectionState.pointFormat)))
				{
					_text += insertOp.text;
					setGenerations(beginGeneration,insertOp.endGeneration);
				}
				else
					return null;
				setGenerations(beginGeneration,insertOp.endGeneration);
				return this;
			}
			
			if (op2 is SplitParagraphOperation)
				return new CompositeOperation([this,op2]);

			return null;
		}
	}
}