////////////////////////////////////////////////////////////////////////////////
//
//  ADOBE SYSTEMS INCORPORATED
//  Copyright 2009 Adobe Systems Incorporated
//  All Rights Reserved.
//
//  NOTICE: Adobe permits you to use, modify, and distribute this file
//  in accordance with the terms of the license agreement accompanying it.
//
////////////////////////////////////////////////////////////////////////////////

package mx.automation.events
{
	
	import flash.events.Event;
	import mx.controls.listClasses.IListItemRenderer; 
	
	/**
	 *  The AdvancedDataGridHeaderShiftEvent class represents event objects that are dispatched 
	 *  when an AdvancedDataGrid control's header item is shifted. 
	 *  
	 *  @langversion 3.0
	 *  @playerversion Flash 9
	 *  @playerversion AIR 1.1
	 *  @productversion Flex 3
	 */
	public class AdvancedDataGridHeaderShiftEvent extends Event
	{
		include "../../core/Version.as";
		
		//--------------------------------------------------------------------------
		//
		//  Class constants
		//
		//--------------------------------------------------------------------------
		
		
		/**
		 *  The AdvancedDataGridEvent.HEADER_RELEASE constant defines the value of the 
		 *  <code>type</code> property of the event object for a 
		 *  <code>headerRelease</code> event, which indicates that the
		 *  user pressed and released the mouse on a column header.
		 * 
		 *  <p>The properties of the event object have the following values:</p>
		 *  <table class="innertable">
		 *     <tr><th>Property</th><th>Value</th></tr>
		 *     <tr><td><code>bubbles</code></td><td>false</td></tr>
		 *     <tr><td><code>cancelable</code></td><td>true</td></tr>
		 *     <tr><td><code>columnIndex</code></td><td> The zero-based index of the 
		 *       item's column in the AdvancedDataGrid object's <code>columns</code> array.</td></tr>
		 *     <tr><td><code>currentTarget</code></td><td>The Object that defines the 
		 *       event listener that handles the event. For example, if you use 
		 *       <code>myButton.addEventListener()</code> to register an event listener, 
		 *       myButton is the value of the <code>currentTarget</code>. </td></tr>
		 *     <tr><td><code>dataField</code></td><td>The name of the field or property in the
		 *       data associated with the column.</td></tr>
		 *     <tr><td><code>itemRenderer</code></td><td>The header renderer that is
		 *       being released.</td></tr>
		 *     <tr><td><code>localX</code></td><td>NaN</td></tr>
		 *     <tr><td><code>reason</code></td><td>null</td></tr>
		 *     <tr><td><code>rowIndex</code></td><td>null</td></tr>
		 *     <tr><td><code>target</code></td><td>The Object that dispatched the event; 
		 *       it is not always the Object listening for the event. 
		 *       Use the <code>currentTarget</code> property to always access the 
		 *       Object listening for the event.</td></tr>
		 *     <tr><td><code>type</code></td><td>AdvancedDataGridEvent.HEADER_RELEASE</td></tr>
		 *  </table>
		 *
		 *  @eventType headerRelease
		 *  
		 *  @langversion 3.0
		 *  @playerversion Flash 9
		 *  @playerversion AIR 1.1
		 *  @productversion Flex 3
		 */
		public static const HEADER_RELEASE:String = "headerRelease";
		//--------------------------------------------------------------------------
		//
		//  Constructor
		//
		//--------------------------------------------------------------------------
		
		/**
		 *  Constructor.
		 *  
		 *  Normally called by the Flex control and not used in application code.
		 *
		 *  @param type The event type; indicates the action that caused the event.
		 *       
		 *  @param movingColumnIndex The data provider index of the item to be selected.
		 *  
		 *  @param oldColumnIndex The zero-based index before the change.
		 *  
		 *  @param newColumnIndex The zero-based index after the change.
		 *  
		 *  @param bubbles Specifies whether the event can bubble
		 *  up the display list hierarchy.
		 *
		 *  @param cancelable Specifies whether the behavior
		 *  associated with the event can be prevented.
		 * 
		 *  @param triggerEvent The event, such as a mouse or keyboard event, that
		 *              triggered the selection action.
		 *  
		 *  @langversion 3.0
		 *  @playerversion Flash 9
		 *  @playerversion AIR 1.1
		 *  @productversion Flex 3
		 */
		public function AdvancedDataGridHeaderShiftEvent(type:String,
														 movingColumnIndex:int = -1,  
														 oldColumnIndex:int = -1,  
														 newColumnIndex:int = -1,  
														 bubbles:Boolean = false,
														 cancelable:Boolean = false,
														 triggerEvent:Event = null
		)
		{
			super(type, bubbles, cancelable);
			
			this.movingColumnIndex = movingColumnIndex;
			this.oldColumnIndex = oldColumnIndex;
			this.newColumnIndex = newColumnIndex;
			this.triggerEvent = triggerEvent;
		}
		
		//--------------------------------------------------------------------------
		//
		//  Properties
		//
		//--------------------------------------------------------------------------
		
		//----------------------------------
		//  triggerEvent
		//----------------------------------
		
		/**
		 *  Event that triggered the item selection event, 
		 *  such as a keyboard or mouse event.
		 * 
		 *  @default null
		 *  
		 *  @langversion 3.0
		 *  @playerversion Flash 9
		 *  @playerversion AIR 1.1
		 *  @productversion Flex 3
		 */
		public var triggerEvent:Event;
		
		
		
		
		/**
		 *  The automationValue string of the item to be selected.
		 *  This is used when the item to be selected is not visible in the control. 
		 *  
		 *  @langversion 3.0
		 *  @playerversion Flash 9
		 *  @playerversion AIR 1.1
		 *  @productversion Flex 3
		 */
		public var itemAutomationValue:String;
		
		
		/**
		 *  The data provider index of the item to be selected.
		 *  
		 *  @langversion 3.0
		 *  @playerversion Flash 9
		 *  @playerversion AIR 1.1
		 *  @productversion Flex 3
		 */
		public var movingColumnIndex:int;
		/**
		 *  The zero-based index before the change.
		 *  
		 *  @langversion 3.0
		 *  @playerversion Flash 9
		 *  @playerversion AIR 1.1
		 *  @productversion Flex 3
		 */
		public var oldColumnIndex:int;
		/**
		 *  The zero-based index after the change.
		 *  
		 *  @langversion 3.0
		 *  @playerversion Flash 9
		 *  @playerversion AIR 1.1
		 *  @productversion Flex 3
		 */
		public var newColumnIndex:int;
		
		
		//--------------------------------------------------------------------------
		//
		//  Overridden methods: Event
		//
		//--------------------------------------------------------------------------
		
		/**
		 *  @private
		 */
		override public function clone():Event
		{
			return new AdvancedDataGridHeaderShiftEvent(type, movingColumnIndex,  
				oldColumnIndex,  
				newColumnIndex,bubbles, cancelable,triggerEvent);
		}
	}
	
}
