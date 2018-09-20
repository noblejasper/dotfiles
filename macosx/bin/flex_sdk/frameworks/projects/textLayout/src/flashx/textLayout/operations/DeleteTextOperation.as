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
	import flashx.textLayout.edit.IMemento;
	import flashx.textLayout.edit.ModelEdit;
	import flashx.textLayout.edit.PointFormat;
	import flashx.textLayout.edit.SelectionManager;
	import flashx.textLayout.edit.SelectionState;
	import flashx.textLayout.edit.TextFlowEdit;
	import flashx.textLayout.edit.TextScrap;
	import flashx.textLayout.elements.FlowLeafElement;
	import flashx.textLayout.elements.LinkElement;
	import flashx.textLayout.elements.ParagraphElement;
	import flashx.textLayout.formats.ITextLayoutFormat;
	import flashx.textLayout.formats.TextLayoutFormat;
	import flashx.textLayout.tlf_internal;
	
	use namespace tlf_internal;

	/**
	 * The DeleteTextOperation class encapsulates the deletion of a range of text.
	 *
	 * @see flashx.textLayout.edit.EditManager
	 * @see flashx.textLayout.events.FlowOperationEvent
	 * 
	 * @playerversion Flash 10
	 * @playerversion AIR 1.5
	 * @langversion 3.0 
	 */
	public class DeleteTextOperation extends FlowTextOperation
	{
		private var _memento:IMemento;
		private var _allowMerge:Boolean;
		private var _pendingFormat:PointFormat;
		
		private var _deleteSelectionState:SelectionState = null;
		/** 
		 * Creates a DeleteTextOperation operation.
		 * 
		 * @param operationState The original range of text.
		 * @param deleteSelectionState The range of text to delete, if different from the range 
		 * described by <code>operationState</code>. (Set to <code>null</code> to delete the range
		 * described by <code>operationState</code>.)
		 * @param allowMerge Set to <code>true</code> if this operation can be merged with the next or previous operation.
		 * 
		 * @playerversion Flash 10
		 * @playerversion AIR 1.5
	 	 * @langversion 3.0 
		 */
		public function DeleteTextOperation(operationState:SelectionState, deleteSelectionState:SelectionState = null, allowMerge:Boolean = false)
		{
			_deleteSelectionState = deleteSelectionState ? deleteSelectionState : operationState;				
			
			super(_deleteSelectionState);
			originalSelectionState = operationState;
			_allowMerge = allowMerge;
		}
		
		/** 
		 * Indicates whether this operation can be merged with operations executed before or after it.
		 * 
		 * <p>Some delete operations, for example, a sequence of backspace keystrokes, can be fruitfully 
		 * merged into one operation so that undoing the operation reverses the entire sequence.</p>
		 * 
		 * @playerversion Flash 10
		 * @playerversion AIR 1.5
	 	 * @langversion 3.0 
		*/
		public function get allowMerge():Boolean
		{
			return _allowMerge;
		}
		public function set allowMerge(value:Boolean):void
		{
			_allowMerge = value;
		}
		
		/** 
		 * deleteSelectionState The range of text to delete
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
		
		/** @private */
		public override function doOperation():Boolean
		{
			// Nothing to delete
			if (absoluteStart == absoluteEnd)
				return false;
				
			_pendingFormat = PointFormat.createFromFlowElement(textFlow.findLeaf(absoluteStart));
			if (_pendingFormat.linkElement)		// don't propagate links or tcy from deleted text
				_pendingFormat.linkElement = null;
			if (_pendingFormat.tcyElement)		// don't propagate links or tcy from deleted text
				_pendingFormat.tcyElement = null;
						
			_memento = ModelEdit.deleteText(textFlow, absoluteStart, absoluteEnd, true);
			
			if (originalSelectionState.selectionManagerOperationState && textFlow.interactionManager)
			{
				// set pointFormat from leafFormat
				var state:SelectionState = textFlow.interactionManager.getSelectionState();
				if (state.anchorPosition == state.activePosition)
				{
					state.pointFormat = PointFormat.clone(_pendingFormat);
					textFlow.interactionManager.setSelectionState(state);
				}
			}

			return true;	
		}
		
		/** @private */
		public override function undo():SelectionState
		{
			if (_memento)
				_memento.undo();
			return originalSelectionState;				
		}
	
		/** @private */
		public override function redo():SelectionState
		{
			if (_memento)
				_memento.redo();
			return new SelectionState(textFlow,absoluteStart,absoluteStart,_pendingFormat);	
		}

		/** @private */
		tlf_internal override function merge(op2:FlowOperation):FlowOperation
		{
			if (this.endGeneration != op2.beginGeneration)
					return null;
			var delOp:DeleteTextOperation = op2 as DeleteTextOperation;
			if ((delOp == null) || !delOp.allowMerge || !_allowMerge)
				return null;
				
			return new CompositeOperation([this, op2]);
		}	
	}
}
