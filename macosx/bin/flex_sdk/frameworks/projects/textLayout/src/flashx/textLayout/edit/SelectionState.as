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
package flashx.textLayout.edit
{
	import flashx.textLayout.elements.TextFlow;
	import flashx.textLayout.elements.TextRange;
	import flashx.textLayout.formats.ITextLayoutFormat;
	import flashx.textLayout.formats.TextLayoutFormat;
	import flashx.textLayout.tlf_internal;
	
	use namespace tlf_internal;
	
	import flashx.textLayout.tlf_internal;
	use namespace tlf_internal;
	/**
	 * The SelectionState class represents a selection in a text flow.  
	 * 
	 * <p>A selection range has an anchor point, representing the point at which the selection of text began, and an
	 * active point, representing the point to which the selection is extended. The active point can be before or after 
	 * the anchor point in the text. If a selection is modified (for example, by a user shift-clicking with the mouse),
	 * the active point changes while the anchor point always remains in the same position.</p>
	 *
	 * @see flashx.textLayout.edit.ISelectionManager#getSelectionState()
	 * @see flashx.textLayout.elements.TextFlow
	 * @see flashx.textLayout.elements.TextRange
	 * 
	 * @playerversion Flash 10
	 * @playerversion AIR 1.5
 	 * @langversion 3.0
 	 */
	public class SelectionState extends TextRange
	{		
		/** Format that are associated with the caret position & will be applied to inserted text */
		private var _pointFormat:ITextLayoutFormat;
		
		private var _selectionManagerOperationState:Boolean;

		/** 
		 * Creates a SelectionState object.
		 * 
		 * <p><b>Note:</b> Do not construct a SelectionState object in order to create a selection. To
		 * create a selection in a text flow, call the <code>setSelection()</code> method of the relevant
		 * ISelectionManager instance (which is the SelectionManager or EditManager object assigned 
		 * to the <code>interactionManager</code> property of the text flow).</p>
		 * 
		 * @param	root	The TextFlow associated with the selection.
		 * @param anchorPosition	The anchor index of the selection.
		 * @param activePosition	The active index of the selection.
		 * @param	format	The TextLayoutFormat to be applied on next character typed when a point selection
		 * 
		 * @see flashx.textLayout.edit.ISelectionManager#getSelectionState()
		 * @see flashx.textLayout.edit.SelectionManager
		 * @see flashx.textLayout.edit.EditManager
		 * 
		 * @playerversion Flash 10
		 * @playerversion AIR 1.5
 	 	 * @langversion 3.0
		 */		
		public function SelectionState(root:TextFlow,anchorPosition:int,activePosition:int,format:ITextLayoutFormat = null)
		{
			super(root, anchorPosition, activePosition);
			if (format)
				_pointFormat = format;
		}
		
		/** 
		 * Updates the selection range with new anchor or active position values.  
		 * 
		 * <p>The <code>pointFormat</code> styles are cleared if the selection is changed.</p>
		 * 
		 * @param newAnchorPosition	the anchor index of the selection.
		 * @param newActivePosition	the active index of the selection.
		 * @return true if selection is changed
		 * 
		 */
		public override function updateRange(newAnchorPosition:int,newActivePosition:int):Boolean
		{
			if (super.updateRange(newAnchorPosition,newActivePosition) )
			{
				_pointFormat = null;
				return true;
			}
			return false;
		}
		
		/** 
		 * The format attributes applied to inserted text. 
		 * 
		 * <p><b>Note:</b> The <code>pointFormat</code> object does not include inherited styles. To
		 * get all the applicable style definitions, use the <code>getCommonCharacterFormat()</code>
		 * method of the ISelectionManager class.</p>
		 * 
		 * @see ISelectionManager#getCommonCharacterFormat()
		 * 
		 * @playerversion Flash 10
		 * @playerversion AIR 1.5
 	 	 * @langversion 3.0
		 */												
		public function get pointFormat():ITextLayoutFormat
		{ return _pointFormat; }
		public function set pointFormat(format:ITextLayoutFormat):void
		{ _pointFormat = format; } 
		
		/** @private used to tell an operation that the SelectionState is from the SelectionManager and that the SelectionManager pointFormat should be updated. */
		tlf_internal function get selectionManagerOperationState():Boolean
		{ return _selectionManagerOperationState; }
		/** @private */		
		tlf_internal function set selectionManagerOperationState(val:Boolean):void
		{ _selectionManagerOperationState = val; }
		/** @private */
		tlf_internal function clone():SelectionState
		{ return new SelectionState(textFlow,anchorPosition,activePosition,pointFormat); }
	}
}