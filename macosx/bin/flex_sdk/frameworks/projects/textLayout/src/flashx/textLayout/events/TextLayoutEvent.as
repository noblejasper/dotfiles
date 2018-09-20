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
	
	/**
	 * A TextLayoutEvent instance represents an event, such as the 
	 * <code>TextLayoutEvent.SCROLL</code> event, that does not require
	 * custom properties. 
	 * <p>A scroll event is represented by a TextLayoutEvent instance with its 
	 * <code>type</code> property set to <code>TextLayoutEvent.SCROLL</code>.
	 * A class specifically for scroll events is not necessary because there are
	 * no custom properties for a scroll event, as there are for the other
	 * events that have specific event classes.
	 * If a new text layout event is needed, and the event does not require
	 * custom properties, the new event will also be represented by a
	 * TextLayoutEvent object, but with its <code>type</code> property
	 * set to a new static constant.
	 * </p>
	 *
	 * @includeExample examples\TextLayoutEvent_example.as -noswf
	 * @playerversion Flash 10
	 * @playerversion AIR 1.5
	 * @langversion 3.0
	 */
	public class TextLayoutEvent extends Event
	{
	    /**
	     *  The <code>TextLayoutEvent.SCROLL</code> constant defines the value of the
	     *  <code>type</code> property of the event object for a <code>scroll</code> event.
	     * @playerversion Flash 10
	     * @playerversion AIR 1.5
	     * @langversion 3.0
	     */
	    public static const SCROLL:String = "scroll";
	    
		/**
		 *  The TextLayoutEvent class represents the event object passed to
		 *  the event listener for many Text Layout events.
		 * @playerversion Flash 10
		 * @playerversion AIR 1.5
		 * @langversion 3.0
		 */
		public function TextLayoutEvent(type:String, bubbles:Boolean=false, cancelable:Boolean=false)
		{
			super(type, bubbles, cancelable);
		}
		
        /** @private */
        override public function clone():Event
        {
        	return new TextLayoutEvent(type, bubbles, cancelable);
        }		
	}
}