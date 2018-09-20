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
	import flashx.textLayout.elements.TextFlow;
	
	/** 
	 * A TextFlow instance dispatches this event after a compose operation completes. 
	 * Each text container has two states: composition and display. This event notifies
	 * you when the composition phase has ended. This provides an opportunity to make any
	 * necessary and appropriate changes to the container before you display the text. 
	 * For example, you can use this event to add highlighting of certain words or
	 * characters in the text flow before the text is displayed.
	 * 
	 * <p>The three main methods that dispatch this event are <code>compose()</code>, 
	 * <code>updateToController()</code>, and <code>updateAllControllers()</code>.
	 * All three of these methods are in the StandardFlowComposer class.</p>
	 *
	 * <p><strong>Note: </strong>If the event is dispatched by the
	 * <code>updateAllControllers()</code> method, do not call 
	 * <code>updateAllControllers()</code> again in your event handler function.
	 * Such a call would be a recursive call because the <code>updateAllControllers()</code> 
	 * method executes both the composition and display steps. The <code>updateAllControllers()</code>
	 * dispatches this event after composition completes, but before the display step executes.
	 * The same reasoning applies to the <code>updateToController()</code> method.
	 * </p>
	 *
	 * @includeExample examples\CompositionCompleteEvent_example.as -noswf
	 *
	 * @see flashx.textLayout.elements.TextFlow 
	 * @playerversion Flash 10
	 * @playerversion AIR 1.5
	 * @langversion 3.0 
	 */
	public class CompositionCompleteEvent extends Event
	{
	    /** 
	     * Defines the value of the <code>type</code> property of a <code>compositionComplete</code> event object 
	     * @playerversion Flash 10
	     * @playerversion AIR 1.5
	     * @langversion 3.0 
	     */
	    public static const COMPOSITION_COMPLETE:String = "compositionComplete";
		
		
		//temporary removal of params until Flex removes references to old CompositionCompletionEvent
		private var _compositionStart:int;
		private var _compositionLength:int;
		private var _textFlow:TextFlow;	
		
		/** Constructor
		 * @param type event type - use the static property COMPOSITION_COMPLETE.
		 * @param bubbles Indicates whether an event is a bubbling event. This event does not bubble.
		 * @param cancelable Indicates whether the behavior associated with the event can be prevented.
		 * This event cannot be cancelled.
		 * @param textFlow The TextFlow which was composed
		 * @param compositionStart start of composition, in terms of an index into the text flow.
		 * @param compositionLength length number of characters composed
		 * @playerversion Flash 10
		 * @playerversion AIR 1.5
		 * @langversion 3.0 
		 **/
		public function CompositionCompleteEvent(type:String, bubbles:Boolean=false, cancelable:Boolean=false, textFlow:TextFlow =  null, compositionStart:int=0, compositionLength:int=0)
		{
			_compositionStart = compositionStart;
			_compositionLength = compositionLength;
			_textFlow = textFlow
			super(type, bubbles, cancelable);
		}
		
      	/** @private */
		override public function clone():Event
		{
			return new CompositionCompleteEvent(type, bubbles, cancelable, textFlow, compositionStart, compositionLength);
		}
		
		/** 
		 * The start location of the text range affected by the composition, expressed as an index into the text flow.
		 * @playerversion Flash 10
		 * @playerversion AIR 1.5
		 * @langversion 3.0 		 
		 */
		public function get compositionStart():int
		{ return _compositionStart; }		
		public function set compositionStart(value:int):void
		{ _compositionStart = value; }		
		
		/** 
		 * The number of characters composed.
		 * @playerversion Flash 10
		 * @playerversion AIR 1.5
		 * @langversion 3.0 
		 */
		public function get compositionLength():int
		{ return _compositionLength; }
		public function set compositionLength(value:int):void
		{ _compositionLength = value; }
		
		/**
		 * TextFlow on which composition has been completed. 
		 * @playerversion Flash 10
		 * @playerversion AIR 1.5
		 * @langversion 3.0
		 */
		public function get textFlow():TextFlow
		{ return _textFlow; }
		public function set textFlow(value:TextFlow):void
		{ _textFlow = value; }
	}
}
