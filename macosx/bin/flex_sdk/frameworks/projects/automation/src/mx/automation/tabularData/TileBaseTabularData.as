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
package mx.automation.tabularData
{
	
	import mx.automation.AutomationManager;
	import mx.automation.IAutomationObject;
	import mx.automation.IAutomationTabularData;
	import mx.collections.CursorBookmark;
	import mx.collections.errors.ItemPendingError;
	import mx.controls.listClasses.TileBase;
	import mx.controls.listClasses.IListItemRenderer;
	import mx.core.mx_internal;
	use namespace mx_internal;
	
	/**
	 *  @private
	 */
	public class TileBaseTabularData extends ListBaseTabularData
	{
		
		private var list:TileBase;
		
		/**
		 *  Constructor
		 *  
		 *  @langversion 3.0
		 *  @playerversion Flash 9
		 *  @playerversion AIR 1.1
		 *  @productversion Flex 3
		 */
		public function TileBaseTabularData(l:TileBase)
		{
			super(l);
			
			list = l;
		}
		
		/**
		 *  @inheritDoc
		 *  
		 *  @langversion 3.0
		 *  @playerversion Flash 9
		 *  @playerversion AIR 1.1
		 *  @productversion Flex 3
		 */
		override public function getAutomationValueForData(data:Object):Array
		{
			var item:IListItemRenderer = list.getListVisibleData()[list.getItemUID(data)];
			
			if (item == null)
			{
				item = list.getMeasuringRenderer(data);
				list.setupRendererFromData(item, data);
			}
			
			var delegate:IAutomationObject = (item as IAutomationObject);
			return [ delegate.automationValue.join(" | ") ];
		}
	}
}
