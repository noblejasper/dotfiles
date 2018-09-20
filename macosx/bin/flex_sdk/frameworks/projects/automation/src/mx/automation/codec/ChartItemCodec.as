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

package mx.automation.codec
{ 
	
	import mx.automation.qtp.IQTPPropertyDescriptor;
	import mx.automation.IAutomationManager;
	import mx.automation.IAutomationObject;
	import mx.charts.HitData;
	import mx.charts.chartClasses.Series;
	
	import mx.core.IFlexDisplayObject;
	import mx.core.mx_internal;
	import mx.charts.ChartItem;
	import mx.charts.chartClasses.IChartElement;
	import mx.charts.chartClasses.ChartBase;
	
	use namespace mx_internal;
	
	/**
	 * translates between internal Flex HitData and automation-friendly version
	 *  
	 *  @langversion 3.0
	 *  @playerversion Flash 9
	 *  @playerversion AIR 1.1
	 *  @productversion Flex 3
	 */
	public class ChartItemCodec extends DefaultPropertyCodec
	{
		//--------------------------------------------------------------------------
		//
		//  Constructor
		//
		//--------------------------------------------------------------------------
		
		public function ChartItemCodec()
		{
			super();
		}
		
		//--------------------------------------------------------------------------
		//
		//  Overridden methods
		//
		//--------------------------------------------------------------------------
		
		override public function encode(automationManager:IAutomationManager,
										obj:Object, 
										propertyDescriptor:IQTPPropertyDescriptor,
										relativeParent:IAutomationObject):Object
		{
			var val:Object = getMemberFromObject(automationManager, obj, propertyDescriptor);
			
			var chartItemData:ChartItem = val as ChartItem;
			
			if (chartItemData)
				return chartItemData.index;
			
			return -1 ;
		}
		
		override public function decode(automationManager:IAutomationManager,
										obj:Object, 
										value:Object,
										propertyDescriptor:IQTPPropertyDescriptor,
										relativeParent:IAutomationObject):void
		{ 
			if (relativeParent is Series)
			{
				var series:Series = relativeParent as Series;
				var items:Array = series.items;
				var n:int = items.length;
				for (var i:int = 0; i < n; ++i)
				{
					if (items[i] is ChartItem)
					{
						var chartItem:ChartItem = items[i] as ChartItem;
						if (chartItem.index == value)
						{
							obj[propertyDescriptor.name] = 
								new HitData(0, 0, 0, 0, chartItem);
							break;
						}
					}
				}
			}
			else if (relativeParent is ChartBase)
			{
				var series1:ChartBase = relativeParent as ChartBase;
				var items1:Array = series1.dataProvider as Array;
				var n1:int = items1.length;
				for (var i1:int = 0; i1 < n1; i1++)
				{
					if (items1[i1] is ChartItem)
					{
						var chartItem1:ChartItem = items1[i1] as ChartItem;
						if ( chartItem1.index == value)
						{
							obj[propertyDescriptor.name] = 
								new HitData(0, 0, 0, 0, chartItem1);
							break;
						}
					}
				}
			}
		}
	}
	
}
