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
	
	import flashx.textLayout.container.ContainerController;
	import flashx.textLayout.elements.TextFlow;

	/** 
	 * A TextFlow instance dispatches this event after any of its containers completes 
	 * an update. Each text container has two states: composition and display. This 
	 * event notifies you when the display phase has ended. This provides an 
	 * opportunity to make any necessary changes to the container when it is ready to
	 * be displayed, but hasn't yet been painted to the screen.
	 * 
	 * @internal Note: the DamageEvent_example class contains a good example of 
	 * using the UpdateCompleteEvent, so I have included it as the class example
	 * instead of creating a new example. I've updated the description of the
	 * DamageEvent_example file to include prominent mention of the UpdateCompleteEvent.
	 *
	 * @includeExample examples\DamageEvent_example.as -noswf
	 * @see flashx.textLayout.elements.TextFlow 
	 * @playerversion Flash 10
	 * @playerversion AIR 1.5
	 * @langversion 3.0 
	 */
	public class UpdateCompleteEvent extends Event
	{
	    /** 
	     * Defines the value of the <code>type</code> property of an <code>UpdateCompleteEvent</code> object 
	     * @playerversion Flash 10
	     * @playerversion AIR 1.5
	     * @langversion 3.0 
	     */
	    public static const UPDATE_COMPLETE:String = "updateComplete";
	    
	    /** @private */
	    private var _controller:ContainerController;
	    /** @private */
		private var _textFlow:TextFlow;
		
		/** Constructor
		 * @param type event type - use the static property UPDATE_COMPLETE.
		 * @param bubbles Indicates whether an event is a bubbling event. This event does not bubble.
		 * @param cancelable Indicates whether the behavior associated with the event can be prevented.
		 * This event cannot be cancelled.
		 * @param controller The ContainerController whose container was updated
		 * @param textFlow The TextFlow which was updated
		 * @playerversion Flash 10
		 * @playerversion AIR 1.5
		 * @langversion 3.0 
		 **/
		public function UpdateCompleteEvent(type:String, bubbles:Boolean=false, cancelable:Boolean=false, textFlow:TextFlow =  null, controller:ContainerController=null)
		{
			super(type, bubbles, cancelable);
			this.controller = controller;
			_textFlow = textFlow;
		}

      	/** @private */
		override public function clone():Event
		{
			return new UpdateCompleteEvent(type, bubbles, cancelable, _textFlow, _controller);
		}

		/** 
		 * The controller of the container being updated
		 * @playerversion Flash 10
		 * @playerversion AIR 1.5
		 * @langversion 3.0 		 
		 */
		public function get controller():ContainerController
		{ return _controller; }
		public function set controller(c:ContainerController):void
		{ _controller = c; }
		
		
		/**
		 * TextFlow which has been updated. 
		 * @playerversion Flash 10
		 * @playerversion AIR 1.5
		 * @langversion 3.0
		 */
		public  function get textFlow():TextFlow
		{ return _textFlow; }
		public  function set textFlow(value:TextFlow):void
		{ _textFlow = value; }
	}
}