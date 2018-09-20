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
	
	import mx.automation.Automation;
	import mx.automation.IAutomationObject;
	import mx.automation.IAutomationObjectHelper;
	import mx.core.mx_internal;
	
	import spark.automation.delegates.components.supportClasses.SparkSkinnableComponentAutomationImpl;
	import spark.components.Scroller;
	
	use namespace mx_internal;
	
	[Mixin]
	/**
	 * 
	 *  Defines methods and properties required to perform instrumentation for the 
	 *  Scroller control.
	 * 
	 *  @see spark.components.Scroller 
	 *
	 */
	public class SparkScrollerAutomationImpl extends SparkSkinnableComponentAutomationImpl
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
		 */
		public static function init(root:DisplayObject):void
		{
			Automation.registerDelegateClass(spark.components.Scroller, SparkScrollerAutomationImpl);
		}   
		
		/**
		 *  Constructor.
		 * @param obj Scroller object to be automated.     
		 */
		public function SparkScrollerAutomationImpl(obj:spark.components.Scroller)
		{
			super(obj);
			
		}
		
		/**
		 *  @private
		 */
		private function get scroller():spark.components.Scroller
		{
			return uiComponent as spark.components.Scroller;
		}
		
		
		
		override public function getAutomationChildren():Array
		{
			
			var chilArray:Array = new Array();
			return chilArray;
		}
		
		
		/**
		 *  @private
		 */
		override public function createAutomationIDPart(child:IAutomationObject):Object
		{
			var help:IAutomationObjectHelper = Automation.automationObjectHelper;
			return help.helpCreateIDPart(uiAutomationObject, child);
		}
		
		/**
		 *  @private
		 */
		override public function resolveAutomationIDPart(part:Object):Array
		{
			var help:IAutomationObjectHelper = Automation.automationObjectHelper;
			return help.helpResolveIDPart(uiAutomationObject, part);
		}
		
		/**
		 *  @private
		 */
		override public function createAutomationIDPartWithRequiredProperties(child:IAutomationObject, properties:Array):Object
		{
			var help:IAutomationObjectHelper = Automation.automationObjectHelper;
			return help.helpCreateIDPartWithRequiredProperties(uiAutomationObject, child,properties);
			
		}
		
		/**
		 *  @private
		 */
		
		override public function get numAutomationChildren():int
		{ 		
			return 0;
		}
		
		/**
		 *  @private
		 */
		
		override public function getAutomationChildAt(index:int):IAutomationObject
		{		
			return null;
		}
		
		
	}
	
}