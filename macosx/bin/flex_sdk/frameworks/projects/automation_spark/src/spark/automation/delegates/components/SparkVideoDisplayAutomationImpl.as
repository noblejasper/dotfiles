////////////////////////////////////////////////////////////////////////////////
//
//  ADOBE SYSTEMS INCORPORATED
//  Copyright 2010 Adobe Systems Incorporated
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
	import mx.automation.delegates.core.UIComponentAutomationImpl;
	import spark.components.VideoDisplay;
		
		[Mixin]
		/**
		 * 
		 *  Defines methods and properties required to perform instrumentation for the 
		 *  VideoDisplay control.
		 * 
		 *  @see spark.components.VideoDisplay 
		 *
		 *  
		 *  @langversion 3.0
		 *  @playerversion Flash 10
		 *  @playerversion AIR 1.1
		 *  @productversion Flex 4.5
		 */
		public class SparkVideoDisplayAutomationImpl extends UIComponentAutomationImpl
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
			 *  @playerversion Flash 10
			 *  @playerversion AIR 1.1
			 *  @productversion Flex 4.5
			 */
			public static function init(root:DisplayObject):void
			{
				Automation.registerDelegateClass(spark.components.VideoDisplay, SparkVideoDisplayAutomationImpl);
			}   
			
			//--------------------------------------------------------------------------
			//
			//  Constructor
			//
			//--------------------------------------------------------------------------
			
			/**
			 *  Constructor.
			 * @param obj VideoDisplay object to be automated.     
			 *  
			 *  @langversion 3.0
			 *  @playerversion Flash 10
			 *  @playerversion AIR 1.1
			 *  @productversion Flex 4.5
			 */
			public function SparkVideoDisplayAutomationImpl(obj:spark.components.VideoDisplay)
			{
				super(obj);
				
				recordClick = true;
			}
			
		}
		
}