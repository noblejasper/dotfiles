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
 
import flash.display.CapsStyle;
import flash.display.Graphics;
import flash.display.GraphicsSolidFill;
import flash.display.GraphicsStroke;
import flash.display.JointStyle;
import flash.events.EventDispatcher;
import flash.geom.Point;
import flash.geom.Rectangle;

import mx.events.PropertyChangeEvent;

/**
 *  The SolidColorStroke class defines the properties for a line. 
 *  
 *  You can define a SolidColorStroke object in MXML, but you must attach that SolidColorStroke to
 *  another object for it to appear in your application. The following example
 *  defines two SolidColorStroke objects and then uses them in the horizontalAxisRenderer
 *  of a LineChart control:
 *  
 *  <pre>
 *  ...
 *  &lt;mx:SolidColorStroke id="ticks" color="0xFF0000" weight="1"/&gt;
 *  &lt;mx:SolidColorStroke id="mticks" color="0x0000FF" weight="1"/&gt;
 *  
 *  &lt;mx:LineChart id="mychart" dataProvider="{ndxa}"&gt;
 *      &lt;mx:horizontalAxisRenderer&gt;
 *          &lt;mx:AxisRenderer placement="bottom" canDropLabels="true"&gt;
 *              &lt;mx:tickStroke&gt;{ticks}&lt;/mx:tickStroke&gt;
 *              &lt;mx:minorTickStroke&gt;{mticks}&lt;/mx:minorTickStroke&gt;
 *          &lt;/mx:AxisRenderer&gt;
 *      &lt;/mx:horizontalAxisRenderer&gt;
 *  &lt;/LineChart&gt;
 *  ...
 *  </pre>
 *  
 *  @mxml
 *
 *  <p>The <code>&lt;mx:SolidColorStroke&gt;</code> tag inherits all the tag attributes
 *  of its superclass, and adds the following tag attributes:</p>
 *
 *  <pre>
 *  &lt;mx:SolidColorStroke
 *    <b>Properties</b>
 *    alpha="1.0"
 *    caps="round|none|square"
 *    color="0x000000"
 *    joints="round|bevel|miter"
 *    miterLimit="3"
 *    pixelHinting="false|true"
 *    scaleMode="normal|none|horizontal|vertical"
 *    weight="1 (<i>in most cases</i>)"
 *  /&gt;
 *  </pre>
 *
 *  @see flash.display.Graphics
 *  
 *  @langversion 3.0
 *  @playerversion Flash 9
 *  @playerversion AIR 1.1
 *  @productversion Flex 3
 */
public class SolidColorStroke extends EventDispatcher implements IStroke
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
     *  @param color Specifies the line color.
     *  The default value is 0x000000 (black).
     *
     *  @param weight Specifies the line weight, in pixels.
     *  The default value is 1.
     *
     *  @param alpha Specifies the alpha value in the range 0.0 to 1.0.
     *  The default value is 1.0 (opaque).
     *
     *  @param pixelHinting Specifies whether to hint strokes to full pixels.
     *  This value affects both the position of anchors of a curve
     *  and the line stroke size itself.
     *  The default value is false.
     *
     *  @param scaleMode A value from the LineScaleMode class
     *  that specifies which scale mode to use.
     *  Valid values are <code>LineScaleMode.HORIZONTAL</code>,
     *  <code>LineScaleMode.NONE</code>, <code>LineScaleMode.NORMAL</code>,
     *  and <code>LineScaleMode.VERTICAL</code>.
     *  This parameter is optional,
     *  with a default value of <code>LineScaleMode.NORMAL</code>. 
     *
     *  @param caps Specifies the type of caps at the end of lines.
     *  Valid values are <code>CapsStyle.ROUND</code>, <code>CapsStyle.SQUARE</code>,
     *  and <code>CapsStyle.NONE</code>.
     *  The default value is <code>CapsStyle.ROUND</code>.
     *
     *  @param joints Specifies the type of joint appearance used at angles.
     *  Valid values are <code>JointStyle.ROUND</code>, <code>JointStyle.MITER</code>,
     *  and <code>JointStyle.BEVEL</code>.
     *  The default value is <code>JointStyle.ROUND</code>.
     *
     *  @param miterLimit Indicates the limit at which a miter is cut off.
     *  Valid values range from 1 to 255.
     *  The default value is 3.
     *  
     *  @langversion 3.0
     *  @playerversion Flash 9
     *  @playerversion AIR 1.1
     *  @productversion Flex 3
     */
    public function SolidColorStroke(color:uint = 0x000000,
                                     weight:Number = 1,
                                     alpha:Number = 1.0,
                                     pixelHinting:Boolean = false,
                                     scaleMode:String = "normal",
                                     caps:String = "round",
                                     joints:String = "round",
                                     miterLimit:Number = 3)
    {
        super();

        this.color = color;
        this._weight = weight;
        this.alpha = alpha;
        this.pixelHinting = pixelHinting;
        this.scaleMode = scaleMode;
        this.caps = caps;
        this.joints = joints;
        this.miterLimit = miterLimit;
    }
    
    //--------------------------------------------------------------------------
    //
    //  Properties
    //
    //--------------------------------------------------------------------------

    //----------------------------------
    //  alpha
    //----------------------------------

    private var _alpha:Number = 0.0;
    
    [Bindable("propertyChange")]
    [Inspectable(category="General")]

    /**
     *  The transparency of a line.
     *  Possible values are 0.0 (invisible) through 1.0 (opaque). 
     *  
     *  @default 1.0. 
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
    
    /**
     *  @private
     */
    public function set alpha(value:Number):void
    {
        var oldValue:Number = _alpha;
        if (value != oldValue)
        {
            _alpha = value;
            dispatchStrokeChangedEvent("alpha", oldValue, value);
        }
    }

    //----------------------------------
    //  caps
    //----------------------------------

    private var _caps:String = CapsStyle.ROUND;
    
    [Bindable("propertyChange")]
    [Inspectable(category="General", enumeration="round,square,none", defaultValue="round")]

    /**
     *  Specifies the type of caps at the end of lines.
     *  Valid values are: <code>CapsStyle.ROUND</code>, <code>CapsStyle.SQUARE</code>,
     *  and <code>CapsStyle.NONE</code>.
     *  
     *  @default CapsStyle.ROUND
     * 
     *  @langversion 3.0
     *  @playerversion Flash 9
     *  @playerversion AIR 1.1
     *  @productversion Flex 3
     */
    public function get caps():String
    {
        return _caps;
    }
    
    public function set caps(value:String):void
    {
        var oldValue:String = _caps;
        if (value != oldValue)
        {
            _caps = value;
            dispatchStrokeChangedEvent("caps", oldValue, value);
        }
    }
    
    //----------------------------------
    //  color
    //----------------------------------

    private var _color:uint = 0x000000;
    
    [Bindable("propertyChange")]
    [Inspectable(category="General", format="Color")]

    /**
     *  The line color. 
     *  
     *  @default 0x000000 (black). 
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
            dispatchStrokeChangedEvent("color", oldValue, value);
        }
    }
    
    //----------------------------------
    //  joints
    //----------------------------------

    private var _joints:String = JointStyle.ROUND;
    
    [Bindable("propertyChange")]
    [Inspectable(category="General", enumeration="round,bevel,miter", defaultValue="round")]

    /**
     *  Specifies the type of joint appearance used at angles.
     *  Valid values are <code>JointStyle.ROUND</code>, <code>JointStyle.MITER</code>,
     *  and <code>JointStyle.BEVEL</code>.
     *  
     *  @default JointStyle.ROUND
     * 
     *  @langversion 3.0
     *  @playerversion Flash 9
     *  @playerversion AIR 1.1
     *  @productversion Flex 3
     */
    public function get joints():String
    {
        return _joints;
    }
    
    public function set joints(value:String):void
    {
        var oldValue:String = _joints;
        if (value != oldValue)
        {
            _joints = value;
            dispatchStrokeChangedEvent("joints", oldValue, value);
        }
    }
    
    //----------------------------------
    //  miterLimit
    //----------------------------------

    private var _miterLimit:Number = 3;
    
    [Bindable("propertyChange")]
    [Inspectable(category="General", minValue="1.0", maxValue="255.0")]
    
    /**
     *  Indicates the limit at which a miter is cut off.
     *  Valid values range from 1 to 255.
     *  
     *  @default 3
     *  
     *  @langversion 3.0
     *  @playerversion Flash 9
     *  @playerversion AIR 1.1
     *  @productversion Flex 3
     */
    public function get miterLimit():Number
    {
        return _miterLimit;
    }
    
    public function set miterLimit(value:Number):void
    {
        var oldValue:Number = _miterLimit;
        if (value != oldValue)
        {
            _miterLimit = value;
            dispatchStrokeChangedEvent("miterLimit", oldValue, value);
        }
    }

    //----------------------------------
    //  pixelHinting
    //----------------------------------

    private var _pixelHinting:Boolean = false;
    
    [Bindable("propertyChange")]
    [Inspectable(category="General")]
    
    /**
     *  Specifies whether to hint strokes to full pixels.
     *  This value affects both the position of anchors of a curve
     *  and the line stroke size itself.
     *  
     *  @default false
     *  
     *  @langversion 3.0
     *  @playerversion Flash 9
     *  @playerversion AIR 1.1
     *  @productversion Flex 3
     */
    public function get pixelHinting():Boolean
    {
        return _pixelHinting;
    }
    
    public function set pixelHinting(value:Boolean):void
    {
        var oldValue:Boolean = _pixelHinting;
        if (value != oldValue)
        {
            _pixelHinting = value;
            dispatchStrokeChangedEvent("pixelHinting", oldValue, value);
        }
    }
    
    //----------------------------------
    //  scaleMode
    //----------------------------------

    private var _scaleMode:String = "normal";
    
    [Bindable("propertyChange")]
    [Inspectable(category="General", enumeration="normal,vertical,horizontal,none", defaultValue="normal")]

     /**
     *  A value from the LineScaleMode class
     *  that  specifies which scale mode to use.
     *  Value valids are:
     * 
     *  <ul>
     *  <li>
     *  <code>LineScaleMode.NORMAL</code>&#151;
     *  Always scale the line thickness when the object is scaled  (the default).
     *  </li>
     *  <li>
     *  <code>LineScaleMode.NONE</code>&#151;
     *  Never scale the line thickness.
     *  </li>
     *  <li>
     *  <code>LineScaleMode.VERTICAL</code>&#151;
     *  Do not scale the line thickness if the object is scaled vertically 
     *  <em>only</em>. 
     *  </li>
     *  <li>
     *  <code>LineScaleMode.HORIZONTAL</code>&#151;
     *  Do not scale the line thickness if the object is scaled horizontally 
     *  <em>only</em>. 
     *  </li>
     *  </ul>
     * 
     *  @default LineScaleMode.NORMAL
     *  
     *  @langversion 3.0
     *  @playerversion Flash 9
     *  @playerversion AIR 1.1
     *  @productversion Flex 3
     */
    public function get scaleMode():String
    {
        return _scaleMode;
    }
    
    public function set scaleMode(value:String):void
    {
        var oldValue:String = _scaleMode;
        if (value != oldValue)
        {
            _scaleMode = value;
            dispatchStrokeChangedEvent("scaleMode", oldValue, value);
        }
    }

    //----------------------------------
    //  weight
    //----------------------------------

    /**
     *  @private
     *  Storage for the weight property.
     */
    private var _weight:Number;

    [Bindable("propertyChange")]
    [Inspectable(category="General", minValue="0.0")]

    /**
     *  The line weight, in pixels.
     *  For many charts, the default value is 1 pixel.
     *  
     *  @langversion 3.0
     *  @playerversion Flash 9
     *  @playerversion AIR 1.1
     *  @productversion Flex 3
     */
    public function get weight():Number
    {
        return _weight;
    }
    
    /**
     *  @private
     */
    public function set weight(value:Number):void
    {
        var oldValue:Number = _weight;
        if (value != oldValue)
        {
            _weight = value;
            dispatchStrokeChangedEvent("weight", oldValue, value);
        }
    }
    
    //--------------------------------------------------------------------------
    //
    //  Methods
    //
    //--------------------------------------------------------------------------

    /**
     *  @inheritDoc
     *  
     *  @langversion 3.0
     *  @playerversion Flash 9
     *  @playerversion AIR 1.1
     *  @productversion Flex 3
     */
    public function apply(graphics:Graphics, targetBounds:Rectangle, targetOrigin:Point):void
    {
        graphics.lineStyle(weight, color, alpha, pixelHinting,
                    scaleMode, caps, joints, miterLimit);
    }
    
    /**
     *  @inheritDoc 
     *  
     *  @langversion 3.0
     *  @playerversion Flash 9
     *  @playerversion AIR 1.1
     *  @productversion Flex 3
     */
    public function createGraphicsStroke(targetBounds:Rectangle, targetOrigin:Point):GraphicsStroke
    {
        // Construct a new GraphicsStroke object and set all of 
        // its properties to match the SolidColorStroke's 
        // properties
        var graphicsStroke:GraphicsStroke = new GraphicsStroke(); 
        graphicsStroke.thickness = weight;  
        graphicsStroke.miterLimit = miterLimit; 
        graphicsStroke.pixelHinting = pixelHinting;
        graphicsStroke.scaleMode = scaleMode;
                
        // There is a bug in Drawing API-2 where if no caps is 
        // specified, a value of 'none' is used instead of 'round'
        graphicsStroke.caps = (!caps) ? CapsStyle.ROUND : caps;
          
        // Give the GraphicsStroke a GraphicsSolidFill corresponding to the 
        // SolidColorStroke's color and alpha values
        var graphicsSolidFill:GraphicsSolidFill = new GraphicsSolidFill();
        graphicsSolidFill.color = color; 
        graphicsSolidFill.alpha = alpha; 
        graphicsStroke.fill = graphicsSolidFill; 
        
        return graphicsStroke;  
    }
    
    /**
     *  @private
     */
    private function dispatchStrokeChangedEvent(prop:String, oldValue:*,
                                                value:*):void
    {
        if (hasEventListener("propertyChange")) 
            dispatchEvent(PropertyChangeEvent.createUpdateEvent(this, prop,
                oldValue, value));
    }
}

}
