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
	import flash.events.Event;
	import mx.automation.Automation; 
	import mx.automation.IAutomationObjectHelper;
	import mx.automation.delegates.TextFieldAutomationHelper;
	import mx.automation.delegates.core.UIComponentAutomationImpl;
	import mx.core.mx_internal;
	import mx.core.IUITextField;
	import mx.controls.Label;
	
	use namespace mx_internal;
	
	[Mixin]
	/**
	 * 
	 *  Defines methods and properties required to perform instrumentation for the 
	 *  Label control.
	 * 
	 *  @see mx.controls.Label 
	 *
	 *  
	 *  @langversion 3.0
	 *  @playerversion Flash 9
	 *  @playerversion AIR 1.1
	 *  @productversion Flex 3
	 */
	public class LabelAutomationImpl extends UIComponentAutomationImpl 
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
			Automation.registerDelegateClass(Label, LabelAutomationImpl);
		}   
		
		//--------------------------------------------------------------------------
		//
		//  Constructor
		//
		//--------------------------------------------------------------------------
		
		/**
		 *  Constructor.
		 * @param obj Label object to be automated.     
		 *  
		 *  @langversion 3.0
		 *  @playerversion Flash 9
		 *  @playerversion AIR 1.1
		 *  @productversion Flex 3
		 */
		public function LabelAutomationImpl(obj:Label)
		{
			super(obj);
		}
		
		//--------------------------------------------------------------------------
		//
		//  Variables
		//
		//--------------------------------------------------------------------------
		
		/**
		 *  @private
		 *  storage for the owner component
		 */
		protected function get label():Label
		{
			return uiComponent as Label;
		}
		
		//--------------------------------------------------------------------------
		//
		//  Overridden properties
		//
		//--------------------------------------------------------------------------
		
		/**
		 *  @private
		 *  Generic record/replay logic for textfields.
		 */
		private var automationHelper:TextFieldAutomationHelper;
		
		//----------------------------------
		//  automationName
		//----------------------------------
		
		/**
		 *  @private
		 */
		override public function get automationName():String
		{
			return label.text || super.automationName;
		}
		
		//----------------------------------
		//  automationValue
		//----------------------------------
		
		/**
		 *  @private
		 */
		override public function get automationValue():Array
		{
			return [ label.text ];
		}
		
		//--------------------------------------------------------------------------
		//
		//  Overridden methods
		//
		//--------------------------------------------------------------------------
		
		/**
		 *  @private
		 */
		override public function replayAutomatableEvent(interaction:Event):Boolean
		{
			return ((automationHelper &&
				automationHelper.replayAutomatableEvent(interaction)) ||
				super.replayAutomatableEvent(interaction));
		}
		
		//--------------------------------------------------------------------------
		//
		//  Event handlers
		//
		//--------------------------------------------------------------------------
		
		/**
		 *  @private
		 */
		override protected function componentInitialized():void
		{
			super.componentInitialized();
			var textField:IUITextField = label.getTextField();
			automationHelper = new TextFieldAutomationHelper(uiComponent, uiAutomationObject, textField);
		}
		
	}
}