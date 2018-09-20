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
	import flash.events.EventDispatcher;
	import flash.events.IEventDispatcher;
	
	import flashx.textLayout.elements.FlowElement;
	import flashx.textLayout.elements.TextFlow;
	import flashx.textLayout.tlf_internal;
	
	use namespace tlf_internal;
	
	[ExcludeClass]
	/** The ElementEventDispatcher class adds a simple tracking counter for event listeners
	 * to the core EventDispatcher class. This variable is tlf_internal, so internal code
	 * can check quickly if an EventDispatcher has any listeners attached at all.
	 */
	
	public class FlowElementEventDispatcher extends EventDispatcher
	{
		/** @private
		 * Internal code can check this counter to quickly determine if there are any
		 * listeners attached to this dispatcher at all.
		 */
		tlf_internal var _listenerCount:int = 0;
		
		/** @private
		 * The element used by this dispatcher
		 */
		tlf_internal var _element:FlowElement;
		
		/** @private 
		 * The constructor is the same as the supeclass constructor.
		 */
		public function FlowElementEventDispatcher(element:FlowElement)
		{
			_element = element;
			super(null);
		}
		
		/** @private 
		 * The addEventListener() method increments a counter.
		 */
		public override function addEventListener(type:String, listener:Function, useCapture:Boolean = false, priority:int = 0, useWeakReference:Boolean = false):void
		{
			super.addEventListener(type, listener, useCapture, priority, useWeakReference);
			_listenerCount++;
			if (_listenerCount == 1)
			{
				var tf:TextFlow = _element.getTextFlow();
				if (tf)
					tf.incInteractiveObjectCount();
			}
			_element.modelChanged(ModelChange.ELEMENT_MODIFIED,_element,0,_element.textLength);
		}
		
		/** @private 
		 * The removeEventListener() method decrements a counter.
		 */
		public override function removeEventListener(type:String, listener:Function, useCapture:Boolean = false):void
		{
			super.removeEventListener(type, listener, useCapture);
			_listenerCount--;			
			if (_listenerCount == 0)
			{
				var tf:TextFlow = _element.getTextFlow();
				if (tf)
					tf.decInteractiveObjectCount();
			}
			_element.modelChanged(ModelChange.ELEMENT_MODIFIED,_element,0,_element.textLength);
		}
	}
}
