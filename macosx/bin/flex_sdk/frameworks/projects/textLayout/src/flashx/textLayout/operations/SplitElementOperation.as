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
	
	import flashx.textLayout.edit.ElementMark;
	import flashx.textLayout.edit.IMemento;
	import flashx.textLayout.edit.ModelEdit;
	import flashx.textLayout.edit.ParaEdit;
	import flashx.textLayout.edit.SelectionState;
	import flashx.textLayout.elements.FlowElement;
	import flashx.textLayout.elements.FlowGroupElement;
	import flashx.textLayout.elements.FlowLeafElement;
	import flashx.textLayout.elements.ListItemElement;
	import flashx.textLayout.elements.ParagraphElement;
	import flashx.textLayout.elements.SpanElement;
	import flashx.textLayout.elements.SubParagraphGroupElementBase;
	import flashx.textLayout.elements.TextFlow;
	import flashx.textLayout.formats.ITextLayoutFormat;
	import flashx.textLayout.formats.ListMarkerFormat;
	import flashx.textLayout.formats.TextLayoutFormat;
	import flashx.textLayout.tlf_internal;


	use namespace tlf_internal;


	/**
	 * The SplitElementOperation class encapsulates a change that splits any FlowGroupElement into two elements.
	 *
	 * This operation splits target at operationState.absoluteStart.
	 * 
	 * @see flashx.textLayout.elements.ParagraphElement
	 * @see flashx.textLayout.edit.EditManager
	 * @see flashx.textLayout.events.FlowOperationEvent
	 * 
	 * @playerversion Flash 10
	 * @playerversion AIR 1.5
	 * @langversion 3.0 
	 */			
	public class SplitElementOperation extends FlowTextOperation
	{
		//range for block delete
		private var delSelOp:DeleteTextOperation;		

		// describes the target
		private var _targetMark:ElementMark;
		
		// moment to undo
		private var _memento:IMemento;
		
		// new element to return to client
		private var _newElement:FlowGroupElement;
		/** 
		 * Creates a SplitElementOperation object.  This operation deletes a block selection and then splits the target at absoluteStart.  The block selection should not cause target to be deleted.
		 * Target is a FlowGroupElement but may not be a LinkElement, TCYElement or SubParagraphGroupElement.
		 * 
		 * @param operationState Describes the point at which to split the element.
		 * If a range of text is specified, the contents of the range are deleted.
		 * 
		 * @playerversion Flash 10
		 * @playerversion AIR 1.5
	 	 * @langversion 3.0 
		 */
		public function SplitElementOperation(operationState:SelectionState, targetElement:FlowGroupElement)
		{
			super(operationState);
			this.targetElement = targetElement;
		}
		
		/** 
		 * Specifies the element this operation modifies.
		 * @playerversion Flash 10
		 * @playerversion AIR 1.5
		 * @langversion 3.0 
		 */
		public function get targetElement():FlowGroupElement
		{
			return _targetMark.findElement(originalSelectionState.textFlow) as FlowGroupElement;
		}
		public function set targetElement(value:FlowGroupElement):void
		{
			_targetMark = new ElementMark(value,0);
		}
		
		/** 
		 * Returns the new element created by doOperation.
		 * @playerversion Flash 10
		 * @playerversion AIR 1.5
		 * @langversion 3.0 
		 */
		public function get newElement():FlowGroupElement
		{ return _newElement; }
		
		/** @private */
		public override function doOperation():Boolean
		{ 
			var target:FlowGroupElement = targetElement;
			
			if (absoluteStart < absoluteEnd)
			{
				// watch out for total deletion of target
				var targStart:int = target.getAbsoluteStart();
				var targEnd:int   = targStart + target.textLength;
				
				delSelOp = new DeleteTextOperation(originalSelectionState);
				delSelOp.doOperation();
				
				if (absoluteStart <= targStart && targEnd <= absoluteEnd)
				{
					// completely deleted
					if (target is ParagraphElement)
						target = textFlow.findAbsoluteParagraph(absoluteStart);
					else
						target = null;	
				}
				else
					target = targetElement;	// recalculate
				
			}
			
			// SubParagraphGroupElements don't split as the target - they just merge again in normalize.  
			// Consider some sort of way to do this.  Generally has to be combined with another operation or somehow marked as don't merge
			// make sure it hasn't been deleted during the delete phase
			if (target != null && !(target is SubParagraphGroupElementBase) && target.getTextFlow() == textFlow)
			{
				var oldLength:int = textFlow.textLength;
				var relativePosition:int = absoluteStart-target.getAbsoluteStart();
				_memento = ModelEdit.splitElement(textFlow,target,relativePosition);
				_newElement = target.parent.getChildAt(target.parent.getChildIndex(target)+1) as FlowGroupElement;

				// fix for 2702736 - when splitting a ListItemElement, make sure not to clone the counterReset marker format - it creates unexpected results, new items don't increment
				if(_newElement is ListItemElement && 
					_newElement.listMarkerFormat && 
					_newElement.listMarkerFormat.counterReset !== undefined)
				{
					var listMarkerFormat:ListMarkerFormat = new ListMarkerFormat(_newElement.listMarkerFormat);
					listMarkerFormat.counterReset = undefined;
					_newElement.listMarkerFormat = listMarkerFormat;
				}
						
				if (textFlow.interactionManager && oldLength != textFlow.textLength && (target is ParagraphElement))
					textFlow.interactionManager.notifyInsertOrDelete(absoluteStart, textFlow.textLength-oldLength);	
			}

			return true;
		}
	
		/** @private */
		public override function undo():SelectionState
		{ 
			if (_memento)
				_memento.undo();	
			_newElement = null;
			
			return absoluteStart < absoluteEnd ? delSelOp.undo() : originalSelectionState;
		}
		
		/** @private */
		public override function redo():SelectionState
		{
			super.redo();
			return textFlow.interactionManager.getSelectionState();
		}
	}
}
