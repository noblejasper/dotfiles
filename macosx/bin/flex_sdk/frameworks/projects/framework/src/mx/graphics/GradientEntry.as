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

package mx.graphics
{

import flash.events.Event;
import flash.events.EventDispatcher;
import mx.events.PropertyChangeEvent;
    
/**
 *  The GradientEntry class defines the objects
 *  that control a transition as part of a gradient fill. 
 *  You use this class with the LinearGradient and RadialGradient classes
 *  to define a gradient fill. 
 *  
 *  @mxml
 *
 *  <p>The <code>&lt;mx:GradientEntry&gt;</code> tag inherits all the tag attributes
 *  of its superclass, and adds the following tag attributes:</p>
 *
 *  <pre>
 *  &lt;mx:GradientEntry
 *    <b>Properties</b>
 *    alpha="1.0"
 *    color="0x000000"
 *    ratio="NaN"
 *  /&gt;
 *  </pre>
 *  
 *  @see mx.graphics.LinearGradient 
 *  @see mx.graphics.RadialGradient
 *  
 *  @langversion 3.0
 *  @playerversion Flash 9
 *  @playerversion AIR 1.1
 *  @productversion Flex 3
 */
public class GradientEntry extends EventDispatcher
{
    include "../core/Version.as";

    //--------------------------------------------------------------------------
    //
    //  Constructor
    //
    //--------------------------------------------------------------------------

    /**
     *  Constructor.
     *
     *  @param color The color for this gradient entry.
     *  The default value is 0x000000 (black).
     *
     *  @param ratio Where in the graphical element the associated color is 
     *  sampled at 100%.
     *  Flex uniformly spaces any GradientEntries
     *  with missing ratio values.
     *  The default value is NaN.
     *
     *  @param alpha The alpha value for this entry in the gradient. 
     *  This parameter is optional. The default value is 1.0.
     *  
     *  @langversion 3.0
     *  @playerversion Flash 9
     *  @playerversion AIR 1.1
     *  @productversion Flex 3
     */
    public function GradientEntry(color:uint = 0x000000,
                                  ratio:Number = NaN,
                                  alpha:Number = 1.0)
    {
        super();

        this.color = color; 
        this.ratio = ratio; 
        this.alpha = alpha;
    }

    //--------------------------------------------------------------------------
    //
    //  Properties
    //
    //--------------------------------------------------------------------------

    //----------------------------------
    //  alpha
    //----------------------------------

    private var _alpha:Number = 1.0;
    
    [Bindable("propertyChange")]
    [Inspectable(category="General", defaultValue="1", minValue="0.0", maxValue="1.0")]

    /**
     *  The transparency of a gradient fill.
     *  Possible values are 0.0 (invisible) through 1.0 (opaque). 
     *  
     *  @default 1.0 
     *  
     *  @langversion 3.0
     *  @playerversion Flash 9
     *  @playerversion AIR 1.1
     *  @productversion Flex 3
     */
    public function get alpha():Number
    {
        return _alpha;
    }
    
    public function set alpha(value:Number):void
    {
        var oldValue:Number = _alpha;
        if (value != oldValue)
        {
            _alpha = value;
            dispatchEntryChangedEvent("alpha", oldValue, value);
        }
    }
    
    //----------------------------------
    //  color
    //----------------------------------

    private var _color:uint;
    
    [Bindable("propertyChange")]
    [Inspectable(category="General", format="Color")]

    /**
     *  The color value for a gradient fill. 
     *  
     *  @langversion 3.0
     *  @playerversion Flash 9
     *  @playerversion AIR 1.1
     *  @productversion Flex 3
     */
    public function get color():uint
    {
        return _color;
    }
    
    public function set color(value:uint):void
    {
        var oldValue:uint = _color;
        if (value != oldValue)
        {
            _color = value;
            dispatchEntryChangedEvent("color", oldValue, value);
        }
    }
    
    //----------------------------------
    //  ratio
    //----------------------------------

    private var _ratio:Number;
    
    [Bindable("propertyChange")]
    [Inspectable(category="General", minValue="0.0", maxValue="1.0")]

    /**
     *  Where in the graphical element, as a percentage from 0.0 to 1.0,
     *  Flex samples the associated color at 100%. 
     *  
     *  @langversion 3.0
     *  @playerversion Flash 9
     *  @playerversion AIR 1.1
     *  @productversion Flex 3
     */
    public function get ratio():Number
    {
        return _ratio;
    }
    
    public function set ratio(value:Number):void
    {
        var oldValue:Number = _ratio;
        if (value != oldValue)
        {
            _ratio = value;
            dispatchEntryChangedEvent("ratio", oldValue, value);
        }
    }

    //--------------------------------------------------------------------------
    //
    //  Methods
    //
    //--------------------------------------------------------------------------

    /**
     *  @private
     */
    private function dispatchEntryChangedEvent(prop:String,
                                               oldValue:*, value:*):void
    {
        if (hasEventListener("propertyChange"))
            dispatchEvent(PropertyChangeEvent.createUpdateEvent(this, prop,
                oldValue, value));
    }
}

}
