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

package spark.automation.delegates.components
{
	import flash.display.DisplayObject;
	import flash.events.Event;
	
	import mx.automation.Automation;
	import mx.automation.IAutomationManager2;
	import mx.automation.IAutomationObject;
	import mx.core.mx_internal;
	import mx.events.WindowExistenceEvent;
	
	import spark.automation.delegates.components.SparkApplicationAutomationImpl;
	import spark.components.WindowedApplication;
	import mx.automation.IAutomationObjectHelper;
	
	use namespace mx_internal;
	
	[Mixin]
	/**
	 * 
	 *  Defines the methods and properties required to perform instrumentation for the 
	 *  SparkWindowedApplication class. 
	 * 
	 *  @see spark.components.WindowedApplication
	 *  
	 *  
	 *  @langversion 3.0
	 *  @playerversion Flash 9
	 *  @playerversion AIR 1.1
	 *  @productversion Flex 4
	 */
	public class SparkWindowedApplicationAutomationImpl extends  SparkApplicationAutomationImpl 
	{
		
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
		 *  @productversion Flex 4
		 */
		public static function init(root:DisplayObject):void
		{
			Automation.registerDelegateClass(spark.components.WindowedApplication, SparkWindowedApplicationAutomationImpl);
		}   
		
		//--------------------------------------------------------------------------
		//
		//  Constructor
		//
		//--------------------------------------------------------------------------
		
		/**
		 *  Constructor.
		 * @param obj WindowedApplication object to be automated.     
		 *  
		 *  @langversion 3.0
		 *  @playerversion Flash 9
		 *  @playerversion AIR 1.1
		 *  @productversion Flex 4
		 */
		public function SparkWindowedApplicationAutomationImpl(obj:spark.components.WindowedApplication)
		{
			super(obj);
			obj.showInAutomationHierarchy = true;
			
			obj.addEventListener(WindowExistenceEvent.WINDOW_CREATE,newWindowHandler);
			obj.addEventListener(WindowExistenceEvent.WINDOW_CREATING,newWindowCreatingHandler);
			obj.addEventListener("menuChanged", menuChangeHandler);
			recordClick = true;
		}
		
		
		/**
		 * @private
		 */
		private function menuChangeHandler(event:Event):void
		{
			Automation.automationManager2.registerNewFlexNativeMenu(windowedApplication.menu, windowedApplication.systemManager as DisplayObject);
			
		}  
		
		/**
		 *  @private
		 */
		private function newWindowHandler(event:WindowExistenceEvent):void
		{
			newIncompleteWindowCount--;
			Automation.automationManager2.registerNewWindow(event.window as DisplayObject);
		}
		
		/**
		 *  @private
		 */
		private var newIncompleteWindowCount:int = 0;
		
		
		/**
		 *  @private
		 */
		private function newWindowCreatingHandler(event:WindowExistenceEvent):void
		{
			var help:IAutomationObjectHelper = Automation.automationObjectHelper;
			newIncompleteWindowCount ++;
			help.addSynchronization(function():Boolean
			{
				return newIncompleteWindowCount==0;
			});
		}
		
		/**
		 *  @private
		 */
		public function get windowedApplication():spark.components.WindowedApplication
		{
			return  uiComponent as spark.components.WindowedApplication;
		}
		
		/**
		 *  @private
		 */
		override public function get automationName():String
		{
			var am:IAutomationManager2 = Automation.automationManager2;
			return am.getUniqueApplicationID();
		}
		
		/**
		 * @private
		 */ 
		override public function get numAutomationChildren():int
		{    	
			//FlexNativeMenu can also be child of WindowedApplication. 
			//So this is added if it exists. We are doing this only for Window and
			//WindowedApplication because at present we receive menuChanged
			//event only on these 2 controls if a FlexNativeMenu is there	
			var count:int = super.numAutomationChildren;
			if (windowedApplication.menu)
				return(count + 1);
			else
				return count;
		}
		
		
		override public function getAutomationChildAt(index:int):IAutomationObject
		{
			//FlexNativeMenu can also be child of WindowedApplication. 
			//So if it exists, it is considered to be the first child.
			// We are doing this only for Window and
			//WindowedApplication because at present we receive menuChanged
			//event only on these 2 controls if a FlexNativeMenu is there	
			if (windowedApplication.menu)
			{
				if (index == 0)
					return windowedApplication.menu as IAutomationObject;
				index = index - 1;
			}
			return super.getAutomationChildAt(index);
		}
		
		
		/**
		 * @private
		 */
		override public function getAutomationChildren():Array
		{
			var childList:Array = new Array();
			
			// add the menu as the first element
			if(windowedApplication.menu)
				childList.push(windowedApplication.menu as IAutomationObject);
			
			var tempList:Array = super.getAutomationChildren();
			if (tempList)
			{
				var n:int  = tempList.length;
				for(var i:int=0; i<n; i++)
				{
					childList.push(tempList[i] as IAutomationObject);
				}
			}
			
			return childList;
		}
		
		
	}
	
}