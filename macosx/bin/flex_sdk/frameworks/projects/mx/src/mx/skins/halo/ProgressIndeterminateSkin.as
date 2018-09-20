////////////////////////////////////////////////////////////////////////////////
//
//  ADOBE SYSTEMS INCORPORATED
//  Copyright 2004-2007 Adobe Systems Incorporated
//  All Rights Reserved.
//
//  NOTICE: Adobe permits you to use, modify, and distribute this file
//  in accordance with the terms of the license agreement accompanying it.
//
////////////////////////////////////////////////////////////////////////////////

package mx.skins.halo
{

import flash.display.Graphics;
import mx.skins.Border;
import mx.styles.StyleManager;
import mx.utils.ColorUtil;

/**
 *  The skin for the indeterminate state of a ProgressBar.
 *  
 *  @langversion 3.0
 *  @playerversion Flash 9
 *  @playerversion AIR 1.1
 *  @productversion Flex 3
 */
public class ProgressIndeterminateSkin extends Border
{
	include "../../core/Version.as";
		
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
	public function ProgressIndeterminateSkin()
	{
		super();
	}	

    //--------------------------------------------------------------------------
    //
    //  Overridden properties
    //
    //--------------------------------------------------------------------------

    //----------------------------------
	//  measuredWidth
    //----------------------------------
    
    /**
     *  @private
     */    
    override public function get measuredWidth():Number
    {
        return 195;
    }
    
    //----------------------------------
	//  measuredHeight
    //----------------------------------
    
    /**
     *  @private
     */        
    override public function get measuredHeight():Number
    {
        return 6;
    }
		
    //--------------------------------------------------------------------------
    //
    //  Overridden methods
    //
    //--------------------------------------------------------------------------

    /**
     *  @private
     */        
	override protected function updateDisplayList(w:Number, h:Number):void
	{
		super.updateDisplayList(w, h);

		// User-defined styles
		var barColorStyle:* = getStyle("barColor");
		var barColor:uint = styleManager.isValidStyleValue(barColorStyle) ?
							barColorStyle :
							getStyle("themeColor");
			
		var barColor0:Number = ColorUtil.adjustBrightness2(barColor, 60);
		var hatchInterval:Number = getStyle("indeterminateMoveInterval");
		
		// Prevents a crash when hatchInterval == 0. Really the indeterminateMoveInterval style should
		// not be hijacked to control the width of the segments on the bar but I'm not sure this is
		// unavoidable while retaining backwards compatibility (see Bug 12942) 
		if (isNaN(hatchInterval) || hatchInterval == 0)
			hatchInterval = 28;

		var g:Graphics = graphics;
		
		g.clear();
		
		// Hatches
		for (var i:int = 0; i < w; i += hatchInterval)
		{
			g.beginFill(barColor0, 0.8);
			g.moveTo(i, 1);
			g.lineTo(Math.min(i + 14, w), 1);
			g.lineTo(Math.min(i + 10, w), h - 1);
			g.lineTo(Math.max(i - 4, 0), h - 1);
			g.lineTo(i, 1);
			g.endFill();
		}
	}
}

}
