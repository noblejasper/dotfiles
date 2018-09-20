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
	import flash.display.InteractiveObject;
	import flash.events.Event;
	import flash.events.FocusEvent;
	import flash.events.KeyboardEvent;
	import flash.events.MouseEvent;
	import flash.events.TextEvent;
	
	import flashx.textLayout.elements.TextFlow;
	import flashx.textLayout.formats.TextLayoutFormat;
	import flashx.textLayout.elements.TextRange;

	/** 
	 * The ISelectionManager interface defines the interface for handling text selection.
	 * 
	 * <p>A SelectionManager keeps track of the selected text range and handles events for a TextFlow.</p>
	 * 
	 * <p>A selection can be either a point selection or a range selection. A point selection is the insertion point
	 * and is indicated visually by drawing a cursor. A range
	 * selection includes the text between an anchor point and an active point.</p>
	 * 
	 * @see flashx.textLayout.edit.SelectionManager
	 * @see flashx.textLayout.edit.TextScrap
	 * @see flashx.textLayout.elements.TextFlow
	 * 
	 * @playerversion Flash 10
	 * @playerversion AIR 1.5
 	 * @langversion 3.0
	 */
	public interface ISelectionManager extends IInteractionEventHandler
	{		
		/** 
		 * The TextFlow object managed by this selection manager. 
		 * 
		 * <p>A selection manager manages a single text flow. A selection manager can also be
		 * assigned to a text flow by setting the <code>interactionManager</code> property of the
		 * TextFlow object.</p>
		 * 
		 * @see flashx.textLayout.elements.TextFlow#interactionManager
		 * 
		 * @playerversion Flash 10
		 * @playerversion AIR 1.5
 		 * @langversion 3.0
		 */
		function get textFlow():TextFlow;
		function set textFlow(flow:TextFlow):void;
		
		/** 
		 * The text position of the start of the selection, as an offset from the start of the text flow.
		 *  
		 * <p>The absolute start is the same as either the active or the anchor point of the selection, whichever comes
		 * first in the text flow.</p>
		 * 
		 * @playerversion Flash 10
		 * @playerversion AIR 1.5
 		 * @langversion 3.0
		 */
		function get absoluteStart() : int;
		/** 
		 * The text position of the end of the selection, as an offset from the start of the text flow.
		 *  
		 * <p>The absolute end is the same as either the active or the anchor point of the selection, whichever comes
		 * last in the text flow.</p>
		 * 
		 * @playerversion Flash 10
		 * @playerversion AIR 1.5
 		 * @langversion 3.0
		 */
		function get absoluteEnd() : int;

		/**
		 * Selects a range of text.
		 * 
		 * <p>If a negative number is passed as either of the parameters, then any existing selection is
		 * removed.</p>
		 * 
		 * @param anchorPosition	The anchor point for the new selection, as an absolute position in the TextFlow 
		 * @param activePosition	The active end of the new selection, as an absolute position in the TextFlow
		 * 
		 * @includeExample examples\SelectionManager_setSelection.as -noswf
		 * 
		 * @playerversion Flash 10
		 * @playerversion AIR 1.5
 		 * @langversion 3.0
		 */
		function selectRange(anchorPosition:int, activePosition:int) : void
		
		/**
		 * Selects the entire flow.
		 * 
		 * @playerversion Flash 10
		 * @playerversion AIR 1.5
 		 * @langversion 3.0
		 */
		function selectAll() : void
		
		/** 
		 * The anchor point of the selection. 
		 * 
		 * <p>An <em>anchor</em> point is the stable end of the selection. When the selection
		 * is extended, the anchor point does not change. The anchor point can be at either the beginning 
		 * or the end of the selection.</p>
		 * 
		 * @playerversion Flash 10
		 * @playerversion AIR 1.5
 		 * @langversion 3.0
		 */								
		function get anchorPosition() : int;

		/** 
		 * The active point of the selection.
		 * 
		 * <p>The <em>active</em> point is the volatile end of the selection. The active point is changed 
		 * when the selection is modified. The active point can be at either the beginning 
		 * or the end of the selection.</p>
		 * 
		 * @playerversion Flash 10
		 * @playerversion AIR 1.5
 		 * @langversion 3.0
		 */										
		function get activePosition() : int;
		
		/**
		 * Indicates whether there is a selection. 
		 * 
		 * <p>Returns <code>true</code> if there is either a range selection or a point selection. 
		 * By default, when a selection manager is first set up, there is no selection (the start and end are -1).</p>
		 * 
		 * @includeExample examples\SelectionManager_hasSelection.as -noswf
		 * 
		 * @playerversion Flash 10
		 * @playerversion AIR 1.5
 		 * @langversion 3.0
		 */
		function hasSelection():Boolean;
		
		/**
		 * Indicates whether the selection covers a range of text.
		 * 
		 * <p>Returns <code>true</code> if there is a selection that extends past a single position.</p> 
		 * 
		 * @includeExample examples\SelectionManager_isRangeSelection.as -noswf
		 * 
		 * @playerversion Flash 10
		 * @playerversion AIR 1.5
 		 * @langversion 3.0
		 */
		function isRangeSelection():Boolean;
		
		/**
		 * Gets the SelectionState object of the current selection.
		 * 
		 * @includeExample examples\SelectionManager_getSelectionState.as -noswf
		 * 
		 * @see flashx.textLayout.edit.SelectionState
		 * 
		 * @playerversion Flash 10
		 * @playerversion AIR 1.5
 	 	 * @langversion 3.0
		 */
		function getSelectionState():SelectionState;
		
		/**
		 * Sets the SelectionState object of the current selection.
		 * 
		 * @see flashx.textLayout.edit.SelectionState
		 * 
		 * @playerversion Flash 10
		 * @playerversion AIR 1.5
		 * @langversion 3.0
		 */
		function setSelectionState(state:SelectionState):void;

		/** 
		 * Redisplays the selection shapes. 
		 * 
		 * <p><b>Note:</b> You do not need to call this method directly. It is called automatically.</p>	
		 * 
		 * @playerversion Flash 10
		 * @playerversion AIR 1.5
 		 * @langversion 3.0
		 */	
		function refreshSelection():void;


		/** 
		 * Gives the focus to the first container in the selection.
		 *  
		 * @includeExample examples\SelectionManager_setFocus.as -noswf
		 * 
		 * @playerversion Flash 10
		 * @playerversion AIR 1.5
 		 * @langversion 3.0
 		 */
		function setFocus():void;
		
		/** 
		 * Indicates whether a container in the text flow has the focus.
		 * 
		 * <p>The <code>focused</code> property is <code>true</code> 
		 * if any of the containers in the text flow has key focus.</p> 
		 * 
		 * @playerversion Flash 10
		 * @playerversion AIR 1.5
 		 * @langversion 3.0
		 */
		function get focused():Boolean;
		
		/** 
		 * Indicates whether the window associated with the text flow is active.
		 * 
		 * <p>The <code>windowActive</code> property is <code>true</code> if the window 
		 * displaying with the text flow is the active window.</p>
		 * 
		 * @playerversion Flash 10
		 * @playerversion AIR 1.5
 		 * @langversion 3.0
		 */
		function get windowActive():Boolean;
		
		/** 
		 * The current SelectionFormat object.
		 * 
		 * <p>The current SelectionFormat object is chosen from the SelectionFormat objects assigned to the 
		 * <code>unfocusedSelectionFormat</code>, <code>inactiveSelectionFormat</code> and <code>focusedSelectionFormat</code> 
		 * properties based on the current state of the <code>windowActive</code> and <code>focused</code> properties.</p> 
		 * 
		 * @playerversion Flash 10
		 * @playerversion AIR 1.5
 		 * @langversion 3.0
		 */
		function get currentSelectionFormat():SelectionFormat;

		/**
		 * Gets the character format attributes that are common to all characters in the specified text range or current selection.
		 * 
		 * <p>Format attributes that do not have the same value for all characters in the specified element range or selection are set to 
		 * <code>null</code> in the returned TextLayoutFormat instance.</p>
		 * 
		 * @param range The optional range of text for which common attributes are requested. If null, the current selection is used. 
		 * @return The common character style settings
		 * 
		 * @includeExample examples\SelectionManager_getCommonCharacterFormat.as -noswf
		 * 
		 * @playerversion Flash 10
		 * @playerversion AIR 1.5
 		 * @langversion 3.0
		 */
		function getCommonCharacterFormat (range:TextRange=null):TextLayoutFormat;
		 
		 /**
		 * Gets the paragraph format attributes that are common to all paragraphs in the specified text range or current selection.
		 * 
		 * <p>Format attributes that do not have the same value for all paragraphs in the specified element range or selection are set to 
		 * <code>null</code> in the returned TextLayoutFormat instance.</p>
		 * 
		 * @param range The optional range of text for which common attributes are requested. If null, the current selection is used. 
		 * @return The common paragraph style settings
		 * 
		 * @includeExample examples\SelectionManager_getCommonParagraphFormat.as -noswf
		 * 
		 * @playerversion Flash 10
		 * @playerversion AIR 1.5
 		 * @langversion 3.0
		 */
		function getCommonParagraphFormat (range:TextRange=null):TextLayoutFormat;
		 
		/**
		 * Gets the container format attributes that are common to all containers in the specified text range or current selection.
		 * 
		 * <p>Format attributes that do not have the same value for all containers in the specified element range or selection are set to 
		 * <code>null</code> in the returned TextLayoutFormat instance.</p>
		 * 
		 * @param range The optional range of text for which common attributes are requested. If null, the current selection is used. 
		 * @return The common container style settings
		 * 
		 * @includeExample examples\SelectionManager_getCommonContainerFormat.as -noswf
		 * 
		 * @playerversion Flash 10
		 * @playerversion AIR 1.5
 		 * @langversion 3.0
		 */
		function getCommonContainerFormat (range:TextRange=null):TextLayoutFormat;

		/**
		 * The editing mode. 
		 * 
		 * <p>The editing mode indicates whether the text flow supports selection, editing, or only reading.
		 * A text flow is made selectable by assigning a selection manager and editable by assigning an edit manager.
		 * Constants representing the editing modes are defined in the EditingMode class.</p>
		 * 
		 * @see flashx.textLayout.EditingMode
		 * 
		 * @playerversion Flash 10
		 * @playerversion AIR 1.5
 		 * @langversion 3.0
		 */
		 function get editingMode():String;		
		
		/**
		 * The SelectionFormat object used to draw the selection in a focused container. 
		 * 
		 * @playerversion Flash 10
		 * @playerversion AIR 1.5
 		 * @langversion 3.0
		 */		 
		 function get focusedSelectionFormat():SelectionFormat;
		 function set focusedSelectionFormat(val:SelectionFormat):void;
		 
		/**
		 * The SelectionFormat object used to draw the selection when it is not in a focused container, but is in
		 * the active window.
		 * 
		 * @playerversion Flash 10
		 * @playerversion AIR 1.5
 		 * @langversion 3.0
		 */		 		 
		 function get unfocusedSelectionFormat():SelectionFormat;
		 function set unfocusedSelectionFormat(val:SelectionFormat):void;
		 
		/**
		 * The SelectionFormat object used to draw the selection when it is not in the active window.
		 * 
		 * @playerversion Flash 10
		 * @playerversion AIR 1.5
 		 * @langversion 3.0
		 */		 		 
		 function get inactiveSelectionFormat():SelectionFormat;
		 function set inactiveSelectionFormat(val:SelectionFormat):void;		 		 
		
		/**
		 * Executes any pending FlowOperations. 
		 * 
		 * <p>The execution of some editing operations, such as text insertion, is delayed 
		 * until the next enterFrame event. Calling <code>flushPendingOperations()</code> causes any deferred operations to be executed 
		 * immediately.</p>
		 * 
		 * @playerversion Flash 10
		 * @playerversion AIR 1.5
 		 * @langversion 3.0
		 * 
		 */
		function flushPendingOperations():void;

		/** 
		 * Updates the selection manager when text is inserted or deleted.
		 * 
		 * <p>Operations must call <code>notifyInsertOrDelete</code> when changing the text in the text flow. 
		 * The selection manager adjusts index-based position indicators accordingly. If you create a new Operation
		 * class that changes text in a text flow directly (not using another operation) your operation must call this function 
		 * to keep the selection up to date.</p>
		 * 
		 * @param absolutePosition	The point in the text where the change was made.
		 * @param length A positive or negative number indicating how many characters were inserted or deleted.
		 * 
		 * @includeExample examples\SelectionManager_notifyInsertOrDelete.as -noswf
		 * 
		 * @playerversion Flash 10
		 * @playerversion AIR 1.5
 	 	 * @langversion 3.0
		 */
		function notifyInsertOrDelete(absolutePosition:int, length:int):void		 
		 	
	}
}
