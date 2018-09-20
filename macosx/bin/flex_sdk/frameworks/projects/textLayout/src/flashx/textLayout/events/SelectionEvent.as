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
package flashx.textLayout.events
{
	import flash.events.Event;
	import flashx.textLayout.edit.SelectionState;
	
	/** 
	 * A TextFlow instance dispatches a SelectionEvent object when
	 * an EditManager or SelectionManager changes or selects a range of text. 
	 * For example, this event is dispatched not only when a range of text is
	 * selected, but also when the selection changes because the
	 * user clicks elsewhere in the text flow. Moreover, this
	 * event is also dispatched when an EditManager changes
	 * the text or text formatting within a range of text.
	 *
	 * @includeExample examples\SelectionEvent_example.as -noswf
	 * @playerversion Flash 10
	 * @playerversion AIR 1.5
	 * @langversion 3.0 
	 */
	public class SelectionEvent extends Event
	{
		/** 
		 * The SelectionEvent.SELECTION_CHANGE constant defines the value of the 
		 * type property of the event object for a selection event. 
		 * @playerversion Flash 10
		 * @playerversion AIR 1.5
		 * @langversion 3.0 
		 */
		public static const SELECTION_CHANGE:String = "selectionChange";
		
		private var _selectionState:SelectionState;
		
		/** 
		 * Creates an event object that contains information about a flow operation.
		 * @param type		The type of the event. Event listeners can access this information through the
		 * inherited <code>type</code> property. There is only one type of SelectionEvent: 
		 * <code>SelectionEvent.SELECTION_CHANGE</code>; 
		 * @param bubbles 	Indicates whether an event is a bubbling event.This event does not bubble.
		 * @param cancelable 	Indicates whether the behavior associated with the event can be prevented.
		 * @param range		An object of type ElementRange that describes the range of text selected.
		 * @playerversion Flash 10
		 * @playerversion AIR 1.5
		 * @langversion 3.0 
		 */
		public function SelectionEvent(type:String, bubbles:Boolean=false, cancelable:Boolean=false, selectionState:SelectionState=null)
		{
			_selectionState = selectionState;
			super(type, bubbles, cancelable);
		}

		/** 
		 * An object of type SelectionState that represents the selected range associated with this SelectionEvent.
		 * 
		 * <p>You can use this property, along with the ElementRange class, to create an ElementRange
		 * instance that represents the range of selected text.
		 * You can use the following line of code to create an instance of the
		 * ElementRange class that represents the range of selected text
		 * (the <code>ev</code> variable represents the event object, and the conditional operator 
		 * is used to guard against a <code>null</code> value for the <code>selectionState</code>
		 * property):</p>
		 * <listing>
		 * // Find selected element range
		 * var range:ElementRange = ev.selectionState ?  
		 * 	ElementRange.createElementRange(ev.selectionState.textFlow,
		 * 	ev.selectionState.absoluteStart, ev.selectionState.absoluteEnd) : null;</listing>
		 * 
		 * 
		 * 
		 * @see flashx.textLayout.edit.ElementRange
		 * @see flashx.textLayout.edit.SelectionState
		 * @playerversion Flash 10
		 * @playerversion AIR 1.5
		 * @langversion 3.0 
		 */		
		public function get selectionState():SelectionState
		{ return _selectionState; }	
		public function set selectionState(value:SelectionState):void
		{ _selectionState = value; }	
		
		
		/** @private */
		override public function clone():Event
		{
			return new SelectionEvent(type, bubbles, cancelable, _selectionState);
		}
		
	}
}