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


package mx.automation.delegates.controls 
{
	import flash.display.DisplayObject;
	
	import mx.automation.Automation;
	import mx.automation.IAutomationObject;
	import mx.automation.delegates.containers.PanelAutomationImpl;
	import mx.controls.Alert;
	import mx.core.mx_internal; 
	import mx.controls.Button;
	
	use namespace mx_internal;
	
	
	
	[Mixin]
	/**
	 * 
	 *  Defines methods and properties required to perform instrumentation for the Alert control. 
	 * 
	 *  @see mx.controls.Alert
	 *  
	 *  
	 *  @langversion 3.0
	 *  @playerversion Flash 9
	 *  @playerversion AIR 1.1
	 *  @productversion Flex 3
	 */
	public class AlertAutomationImpl extends PanelAutomationImpl 
	{
		include "../../../core/Version.as";
		
		//--------------------------------------------------------------------------
		//
		//  Class methods
		//
		//--------------------------------------------------------------------------
		
		/**
		 *  Registers the delegate class for a component class with automation manager.
		 *  
		 *  @param root The SystemManger of the application.
		 *  
		 *  @langversion 3.0
		 *  @playerversion Flash 9
		 *  @playerversion AIR 1.1
		 *  @productversion Flex 3
		 */
		public static function init(root:DisplayObject):void
		{
			Automation.registerDelegateClass(Alert, AlertAutomationImpl);
		}   
		
		/**
		 *  Constructor.
		 * @param obj Alert object to be automated.     
		 *  
		 *  @langversion 3.0
		 *  @playerversion Flash 9
		 *  @playerversion AIR 1.1
		 *  @productversion Flex 3
		 */
		public function AlertAutomationImpl(obj:Alert)
		{
			super(obj);
			alert = obj;
		}
		
		/**
		 *  @private
		 *  storage for the owner component
		 */
		protected var alert:Alert;
		
		//--------------------------------------------------------------------------
		//
		//  Overridden properties
		//
		//--------------------------------------------------------------------------
		
		//----------------------------------
		//  automationName
		//----------------------------------
		
		/**
		 *  @private
		 */
		override public function get automationName():String
		{
			return alert.title || super.automationName;
		}
		
		//----------------------------------
		//  numAutomationChildren
		//----------------------------------
		
		/**
		 *  @private
		 */
		override public function get numAutomationChildren():int
		{
			return super.numAutomationChildren + alert.alertForm.buttons.length;
		}
		
		/**
		 *  @private
		 */
		override public function getAutomationChildAt(index:int):IAutomationObject
		{
			if (index < super.numAutomationChildren)
				return super.getAutomationChildAt(index);
			
			var button:Button = alert.alertForm.buttons[index - super.numAutomationChildren] as Button;
			return button as IAutomationObject;
		}
		
		
		/**
		 * @private
		 */
		override public function getAutomationChildren():Array
		{
			// get the basic children
			var childList:Array = new Array();
			var tempArray1:Array = super.getAutomationChildren();
			var n:int = 0;
			var i:int = 0
			if (tempArray1)
			{
				n = tempArray1.length;
				for(i = 0; i < n ; i++)
				{
					childList.push(tempArray1[i]);
				}
			}
			
			// get the buttons on the form
			var tempArray:Array =  alert.alertForm.buttons;
			n = tempArray?tempArray.length:0;
			
			for (i = 0; i< n ; i++)
			{
				childList.push(tempArray[i]);
			}
			
			return childList;
		}
	}
}