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


package spark.components.mediaClasses
{

import flash.display.DisplayObject;
import flash.display.DisplayObjectContainer;
import flash.display.InteractiveObject;
import flash.events.Event;

import mx.core.IVisualElement;

import spark.components.HSlider;

/**
 *  The VideoScrubBar class defines a video timeline that shows the
 *  current play head location in the video, the amount of the video previously played, 
 *  and the loaded in part of the video.  
 *  The timeline appears at the bottom of the VideoPlayer control.
 *
 *  @see spark.components.VideoPlayer 
 *  
 *  @langversion 3.0
 *  @playerversion Flash 10
 *  @playerversion AIR 1.5
 *  @productversion Flex 4
 */  
public class ScrubBar extends HSlider
{
    
    //--------------------------------------------------------------------------
    //
    //  Constructor
    //
    //--------------------------------------------------------------------------

    /**
     *  Constructor. 
     *  
     *  @langversion 3.0
     *  @playerversion Flash 10
     *  @playerversion AIR 1.5
     *  @productversion Flex 4
     */
    public function ScrubBar()
    {
        super();
        
        dataTipFormatFunction = formatTimeValue;
    }
    
    //--------------------------------------------------------------------------
    //
    //  Skin Parts
    //
    //--------------------------------------------------------------------------
    
    [SkinPart(required="false")]
    
    /**
     *  An optional skin part for the area on the track 
     *  representing the video that's been played.
     *  
     *  @langversion 3.0
     *  @playerversion Flash 10
     *  @playerversion AIR 1.5
     *  @productversion Flex 4
     */
    public var playedArea:IVisualElement;
    
    [SkinPart(required="false")]
    
    /**
     *  An optional skin part for the area on the track 
     *  representing the currently loaded in part of the video.
     * 
     *  <p>For a progressive download video, this will correspond 
     *  to the number of bytes downloaded.  For a streaming video, 
     *  the whole video is "loaded in" as it's quick to seek to 
     *  any spot in the video.</p>
     *  
     *  @langversion 3.0
     *  @playerversion Flash 10
     *  @playerversion AIR 1.5
     *  @productversion Flex 4
     */
    public var loadedRangeArea:IVisualElement;
    
    //--------------------------------------------------------------------------
    //
    // Properties
    //
    //--------------------------------------------------------------------------
    
    //--------------------------------- 
    // loadedRangeEnd
    //---------------------------------
    
    private var _loadedRangeEnd:Number;
    
    /**
     *  The range of currently loaded in values.  This 
     *  property corresponds to the end of that range.
     * 
     *  <p>For a progressive download video, this will correspond 
     *  to the number of bytes downloaded.  For a streaming video, 
     *  the whole video is "loaded in" as it's quick to seek to 
     *  any spot in the video.</p>
     *  
     *  @langversion 3.0
     *  @playerversion Flash 10
     *  @playerversion AIR 1.5
     *  @productversion Flex 4
     */
    public function get loadedRangeEnd():Number
    {
        return _loadedRangeEnd;
    }
    
    /**
     *  @private
     */
    public function set loadedRangeEnd(value:Number):void
    {
        if (value == _loadedRangeEnd)
            return;
        
        _loadedRangeEnd = value;
        invalidateDisplayList();
    }

    //--------------------------------------------------------------------------
    //
    // Methods
    //
    //--------------------------------------------------------------------------
    
    /**
     *  @private
     */
    override protected function partAdded(partName:String, instance:Object):void
    {
        super.partAdded(partName, instance);
        
        if (instance == playedArea)
        {
            if (playedArea is InteractiveObject)
                InteractiveObject(playedArea).mouseEnabled = false;
            if (playedArea is DisplayObjectContainer)
                DisplayObjectContainer(playedArea).mouseChildren = false;
            
            invalidateDisplayList();
        }
        else if (instance == loadedRangeArea)
        {
            if (loadedRangeArea is InteractiveObject)
                InteractiveObject(loadedRangeArea).mouseEnabled = false;
            if (loadedRangeArea is DisplayObjectContainer)
                DisplayObjectContainer(loadedRangeArea).mouseChildren = false;
            
            invalidateDisplayList();
        }
    }
    
    /**
     *  @private
     */
    private function calculateAreaSize(value:Number):Number
    {
        var trackPos:Number = track.getLayoutBoundsX();
        var trackSize:Number = track.getLayoutBoundsWidth();
        var thumbSize:Number = thumb.getLayoutBoundsWidth();
        var range:Number = maximum - minimum;
        var thumbPos:Number = (range > 0) ? (value - minimum) * ((trackSize - thumbSize) / range) : 0;
        return thumbSize + thumbPos;
    }
    
    /**
     *  @private
     */
    override protected function updateSkinDisplayList():void
    {
        super.updateSkinDisplayList();
        
        if (!thumb || !track)
            return;
        
        sizeLoadedRangeArea(calculateAreaSize(loadedRangeEnd));
        sizePlayedArea(calculateAreaSize(pendingValue));
    }
    
    /**
     *  Sets the size of the loaded range area.
     *  The loaded range area defines the area on the timeline that represents 
     *  the currently loaded portion of the video.
     *
     *  @param loadedRangeAreaSize The new size of the loaded in range area.
     *  
     *  @langversion 3.0
     *  @playerversion Flash 10
     *  @playerversion AIR 1.5
     *  @productversion Flex 4
     */
    protected function sizeLoadedRangeArea(loadedRangeAreaSize:Number):void
    {
        if (loadedRangeArea)
            loadedRangeArea.setLayoutBoundsSize(Math.round(loadedRangeAreaSize), NaN);
    }
    
    /**
     *  Sets the size of the played area.
     *  The played area defines the area on the timeline that represents 
     *  the part of the video that has been played. 
     *
     *  @param playedAreaSize The new size of the played area.
     *  
     *  @langversion 3.0
     *  @playerversion Flash 10
     *  @playerversion AIR 1.5
     *  @productversion Flex 4
     */
    protected function sizePlayedArea(playedAreaSize:Number):void
    {
        if (playedArea)
            playedArea.setLayoutBoundsSize(Math.round(playedAreaSize), NaN);            
    }
    
    /**
     *  @private
     */
    private function formatTimeValue(value:Number):String
    {
        // default format: hours:minutes:seconds
        var hours:uint = Math.floor(value/3600) % 24;
        var minutes:uint = Math.floor(value/60) % 60;
        var seconds:uint = Math.round(value) % 60;
        
        var result:String = "";
        if (hours != 0)
            result = hours + ":";
        
        if (result && minutes < 10)
            result += "0" + minutes + ":";
        else
            result += minutes + ":";
        
        if (seconds < 10)
            result += "0" + seconds;
        else
            result += seconds;
        
        return result;
    }
}
}
