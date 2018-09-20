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
	import flashx.textLayout.edit.SelectionState;
	import flashx.textLayout.tlf_internal;

	use namespace tlf_internal;

	//
	// Considered several ways of doing undo/redo
	// 1 - object model level - stashing copies of all changed objects in the model and restoring them
	// 2 - cookies - saving an audit trail of every modified property of the model objects
	// 3 - operations - each operation creates an object that knows how to do/undo/redo itself
	// going with # 3 for now
	//
	import flashx.textLayout.elements.TextFlow;
	import flashx.textLayout.edit.IEditManager;
	import flashx.undo.IOperation;

	/** 
	 * The FlowOperation class is the base class for all Text Layout Framework operations. 
	 * 
	 * <p>Operations are transformations of a text flow. An Operation class defines the
	 * logic for performing and undoing the transformation. Operations are executed by an
	 * edit manager. Most applications do not need to create or manage operations directly
	 * (unless implementing a custom edit manager).</p>
	 * 
	 * <p>When an operation is performed, the edit manager dispatches an Operation object 
	 * within the FlowOperationEvent object. You can query 
	 * this Operation object to decide whether or not to allow the operation, to decide whether 
	 * to perform some other operation as well, or to update related user-interface elements.</p>
	 * 
	 * @see flashx.textLayout.events.FlowOperationEvent
	 * @see flashx.textLayout.edit.EditManager
	 * 
	 * @playerversion Flash 10
	 * @playerversion AIR 1.5
	 * @langversion 3.0 
	 */
	public class FlowOperation implements IOperation
	{
		/** 
		 * Arbitrary data associated with an element. 
		 * 
		 * @playerversion Flash 10
		 * @playerversion AIR 1.5
	 	 * @langversion 3.0 
		 */
		public var userData:*;
		
		// uint or null
		private var _beginGeneration:uint;
		private var _endGeneration:uint;
		
		private var _textFlow:TextFlow;		// target of the operation

		/** 
		 * Creates the FlowOperation object.
		 * 
		 * @param textFlow	The text flow to which this operation is applied.
		 * 
		 * @playerversion Flash 10
		 * @playerversion AIR 1.5
	 	 * @langversion 3.0 
		 */
		public function FlowOperation(textFlow:TextFlow)
		{
			_textFlow = textFlow;
		}
		
		/** 
		 * The TextFlow object to which this operation is applied.
		 * 
		 * @playerversion Flash 10
		 * @playerversion AIR 1.5
	 	 * @langversion 3.0 
		 */
		public function get textFlow():TextFlow
		{ return _textFlow; }
		public function set textFlow(value:TextFlow):void
		{ _textFlow = value; }
		
		/** 
		 * Executes the operation. 
		 * 
		 * <p>This method must be overridden in derived classes. The base class method does nothing.
		 * You should not call <code>doOperation()</code> directly. The edit manager 
		 * calls the method when it executes the operation. </p>
		 * 
		 * @return Boolean <code>true</code>, if the operation succeeded. Otherwise, <code>false</code>.
		 * 
		 * @playerversion Flash 10
		 * @playerversion AIR 1.5
	 	 * @langversion 3.0 
		 */
		public function doOperation():Boolean
		{
			return false;
		}
		
		/**	
		 * Reverses the operation. 
		 * 
		 * <p>This method must be overridden in derived classes. The base class method does nothing.
		 * You should not call <code>undo()</code> directly. The edit manager 
		 * calls the method when it reverses the operation. </p>
		 * 
		 * @return The SelectionState object passed to the operation when it was performed. This
		 * SelectionState object can be the current selection or a selection created specifically
		 * for the operation. 
		 * 
		 * @playerversion Flash 10
		 * @playerversion AIR 1.5
	 	 * @langversion 3.0 
		 */
		public function undo():SelectionState
		{
			return null;
		}
		
		/**	
		 * Test if this operation be placed on the undo stack.
		 * 
		 * @return true means to push the operation onto the undo stack.  false means do not push this operation.
		 * 
		 * @playerversion Flash 10
		 * @playerversion AIR 1.5
		 * @langversion 3.0 
		 */
		public function canUndo():Boolean
		{ return true; }
		
		/**	
		 * Re-executes the operation. 
		 * 
		 * <p>This method must be overridden in derived classes. The base class method does nothing.
		 * You should not call <code>redo()</code> directly. The edit manager 
		 * calls the method when it re-executes the operation. </p>
		 * 
		 * @return The SelectionState object passed to the operation when it was performed. This
		 * SelectionState object can be the current selection or a selection created specifically
		 * for the operation. 
		 * 
		 * @playerversion Flash 10
		 * @playerversion AIR 1.5
	 	 * @langversion 3.0 
		 */
		public function redo():SelectionState
		{
			return null;
		}
		
		// Generation numbers
		
		/**
		 * The text flow generation before the operation.
		 *   
		 * <p>A generation of 0 indicates that the operation did not complete.</p>
		 * 
		 * @playerversion Flash 10
		 * @playerversion AIR 1.5
	 	 * @langversion 3.0 
		*/
		public function get beginGeneration():uint
		{ return _beginGeneration; }
		/** 
		 * The text flow generation after the operation.
		 * 
		 * <p>A generation of 0 indicates that the operation did not complete.</p>
		 * 
		 * @playerversion Flash 10
		 * @playerversion AIR 1.5
	 	 * @langversion 3.0 
		*/
		public function get endGeneration():uint
		{ return _endGeneration; }

		/** @private */
		public function performUndo():void
		{
			var editManager:IEditManager = textFlow ? textFlow.interactionManager as IEditManager : null;
			if (editManager != null)
			{
				editManager.performUndo(this);
			}
		}

		/** @private */
		public function performRedo():void
		{
			var editManager:IEditManager = textFlow ? textFlow.interactionManager as IEditManager : null;
			if (editManager != null)
			{
				editManager.performRedo(this);
			}
		}
		
		/** @private -- Sets the generation numbers into the operation.  */
		tlf_internal function setGenerations(beginGeneration:uint,endGeneration:uint):void
		{
			_beginGeneration = beginGeneration;
			_endGeneration   = endGeneration;
		}
		
		/**
		 * @private
		 * 
		 *  Combine this operation with another operation if the result can 
		 *  be represented as a single operation.  In general, operations cannot be 
		 *  merged. But sequential inserts or deletes may be mergeable.
		 * 
		 *  Merging may occur through updating the properties of the operation
		 *  on which this method is called, by creating a new operation.
		 * 
		 *  @param operation 	The FlowOperation to merge against
		 *  @return A FlowOperation representing the combined operation if 
		 *  the merge was successful, null otherwise.
		 */
		tlf_internal function merge(operation:FlowOperation):FlowOperation	// No PMD
		{
			return null;
		}
	}
}