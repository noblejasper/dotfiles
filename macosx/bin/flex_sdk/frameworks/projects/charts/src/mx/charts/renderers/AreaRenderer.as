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

package mx.charts.renderers
{

import flash.display.Graphics;
import flash.geom.Rectangle;

import mx.charts.chartClasses.GraphicsUtilities;
import mx.core.IDataRenderer;
import mx.graphics.IFill;
import mx.graphics.IStroke;
import mx.graphics.SolidColorStroke;
import mx.skins.ProgrammaticSkin;

/**
 *  The default class used to render the area
 *  beneath the dataPoints of an AreaSeries object.
 *  This class renders the area using the fill, stroke, and line type
 *  as specified by the AreaSeries object's <code>areaFill</code>, <code>areaStroke</code>,
 *  and <code>form</code> styles, respectively.
 *  
 *  @langversion 3.0
 *  @playerversion Flash 9
 *  @playerversion AIR 1.1
 *  @productversion Flex 3
 */
public class AreaRenderer extends ProgrammaticSkin implements IDataRenderer
{
    include "../../core/Version.as";

    //--------------------------------------------------------------------------
    //
    //  Class variables
    //
    //--------------------------------------------------------------------------

	/**
	 *  @private
	 */
	private static var noStroke:SolidColorStroke = new SolidColorStroke(0, 0, 0);

    //--------------------------------------------------------------------------
    //
    //  Constructor
    //
    //--------------------------------------------------------------------------

	/**
	 *  Constructor.
	 *  
	 *  @langversion 3.0
	 *  @playerversion Flash 9
	 *  @playerversion AIR 1.1
	 *  @productversion Flex 3
	 */
	public function AreaRenderer() 
	{
		super();
	}
	
    //--------------------------------------------------------------------------
    //
    //  Properties
    //
    //--------------------------------------------------------------------------

    //----------------------------------
	//  data
    //----------------------------------

	/**
	 *  @private
	 *  Storage for the data property.
	 */
	private var _data:Object;

	[Inspectable(environment="none")]

	/**
	 *  The data that the AreaRenderer renders.
	 *  The AreaRenderer expects this property to be assigned an instance
	 *  of mx.charts.series.renderData.AreaRenderData.
	 *  
	 *  @langversion 3.0
	 *  @playerversion Flash 9
	 *  @playerversion AIR 1.1
	 *  @productversion Flex 3
	 */
	public function get data():Object
	{
		return _data;
	}
	
	/**
	 *  @private
	 */
	public function set data(value:Object):void
	{
		_data = value;

		invalidateDisplayList();
	}
	
    //--------------------------------------------------------------------------
    //
    //  Overridden methods
    //
    //--------------------------------------------------------------------------

	/**
	 *  @private
	 */
	override protected function updateDisplayList(unscaledWidth:Number,
												  unscaledHeight:Number):void
	{
		super.updateDisplayList(unscaledWidth, unscaledHeight);
	
		var fill:IFill = GraphicsUtilities.fillFromStyle(getStyle("areaFill"));
		var stroke:IStroke = getStyle("areaStroke");
		var form:String = getStyle("form");

		var g:Graphics = graphics;
		g.clear();
		
		if (!_data)
			return;
			
		var boundary:Array /* of Object */ = _data.filteredCache;
		var n:int = boundary.length;
		if (n == 0)
			return;
			
		var xMin:Number;
		var xMax:Number = xMin = boundary[0].x;
		var yMin:Number;
		var yMax:Number = yMin = boundary[0].y;

		var v:Object;
		
		for (var i:int = 0; i < n; i++)
		{
			v = boundary[i];
			
			xMin = Math.min(xMin, v.x);
			yMin = Math.min(yMin, v.y);
			xMax = Math.max(xMax, v.x);
			yMax = Math.max(yMax, v.y);
			
			if (!isNaN(v.min))
			{
				yMin = Math.min(yMin, v.min);
				yMax = Math.max(yMax, v.min);
			}
		}

		if (fill)
			fill.begin(g, new Rectangle(xMin, yMin, xMax - xMin, yMax - yMin),null);
		
		GraphicsUtilities.drawPolyLine(g, boundary, 0, n,
										"x", "y", stroke, form);
		
		g.lineStyle(0,0,0);	
			
		if (boundary[0].element.minField != null && boundary[0].element.minField != "")
		{
			g.lineTo(boundary[n - 1].x, boundary[n - 1].min);		
			
			GraphicsUtilities.drawPolyLine(g, boundary, n - 1, -1,
											"x", "min", noStroke, form, false);
		}
		else
		{
			g.lineTo(boundary[n - 1].x, _data.renderedBase);		
			g.lineTo(boundary[0].x, _data.renderedBase);
		}

		g.lineStyle(0, 0, 0);
		g.lineTo(boundary[0].x, boundary[0].y);

		g.endFill();
	}
}

}
