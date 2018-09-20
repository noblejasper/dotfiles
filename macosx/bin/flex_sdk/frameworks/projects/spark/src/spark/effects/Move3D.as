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

package spark.effects
{
import mx.core.mx_internal;
import mx.effects.IEffectInstance;

import spark.effects.animation.MotionPath;
import spark.effects.supportClasses.AnimateTransformInstance;
    
use namespace mx_internal;
    
//--------------------------------------
//  Excluded APIs
//--------------------------------------

[Exclude(name="motionPaths", kind="property")]

/**
 *  The Move3D class moves a target object in the x, y, and z dimensions.
 *  The x, y, and z property specifications of the Move3D effect specify 
 *  the change in x, y, and z that should occur to the transform center around
 *  which the overall transform effect occurs. 
 * 
 *  <p>Like all AnimateTransform-based effects, this effect only works on subclasses
 *  of UIComponent and GraphicElement, as these effects depend on specific
 *  transform functions in those classes. 
 *  Also, transform effects running in parallel on the same target run as a single
 *  effect instance
 *  Therefore, the transform effects share the transform center 
 *  set by any of the contributing effects.</p>
 *  
 *  @mxml
 *
 *  <p>The <code>&lt;s:Move3D&gt;</code> tag
 *  inherits all of the tag attributes of its superclass,
 *  and adds the following tag attributes:</p>
 *
 *  <pre>
 *  &lt;s:Move3D
 *    <b>Properties</b>
 *    id="ID"
 *    xBy="no default"
 *    xFrom="no default"
 *    xTo="no default"
 *    yBy="no default"
 *    yFrom="no default"
 *    yTo="no default"
 *    zBy="no default"
 *    zFrom="no default"
 *    zTo="no default"
 *  /&gt;
 *  </pre>
 *
 *  @see spark.effects.Move
 *
 *  @includeExample examples/Move3DEffectExample.mxml
 *
 *  @langversion 3.0
 *  @playerversion Flash 10
 *  @playerversion AIR 1.5
 *  @productversion Flex 4
 */   
public class Move3D extends AnimateTransform3D
{
    include "../core/Version.as";
    
    //--------------------------------------------------------------------------
    //
    //  Class constants
    //
    //--------------------------------------------------------------------------

    /**
     *  @private
     */
    private static var AFFECTED_PROPERTIES:Array =
        ["translationX", "translationY", "translationZ", 
         "postLayoutTranslationX","postLayoutTranslationY","postLayoutTranslationZ",
         "left", "right", "top", "bottom",
         "horizontalCenter", "verticalCenter", "baseline",
         "width", "height"];

    private static var RELEVANT_STYLES:Array = 
        ["left", "right", "top", "bottom",
         "horizontalCenter", "verticalCenter", "baseline"];

    //--------------------------------------------------------------------------
    //
    //  Constructor
    //
    //--------------------------------------------------------------------------

    /** 
     *  Constructor.
     *
     *  @param target The Object to animate with this effect.
     * 
     *  @langversion 3.0
     *  @playerversion Flash 10
     *  @playerversion AIR 1.5
     *  @productversion Flex 4
     */
    public function Move3D(target:Object=null)
    {
        super(target);
        applyChangesPostLayout = true;
        applyLocalProjection = true;
        instanceClass = AnimateTransformInstance;
        transformEffectSubclass = true;
    }
        
    //--------------------------------------------------------------------------
    //
    //  Properties
    //
    //--------------------------------------------------------------------------

    //----------------------------------
    //  xBy
    //----------------------------------

    [Inspectable(category="General", defaultValue="NaN")]

    /** 
     *  Number of pixels by which to modify the x position of the target.
     *  Values may be negative.
     *  
     *  @langversion 3.0
     *  @playerversion Flash 10
     *  @playerversion AIR 1.5
     *  @productversion Flex 4
     */
    public var xBy:Number;

    //----------------------------------
    //  xFrom
    //----------------------------------

    [Inspectable(category="General", defaultValue="NaN")]

    /** 
     *  Initial x position of the target, in pixels.
     *  If omitted, Flex uses either the value in the starting view state,
     *  if the effect is playing in a transition, or the current
     *  value of the target.
     *  
     *  @langversion 3.0
     *  @playerversion Flash 10
     *  @playerversion AIR 1.5
     *  @productversion Flex 4
     */
    public var xFrom:Number;
    
    //----------------------------------
    //  xTo
    //----------------------------------

    [Inspectable(category="General", defaultValue="NaN")]

    /** 
     *  Final x, in pixels.
     *  If omitted, Flex uses either the value in the starting state,
     *  if the effect is playing in a state transition, or the current
     *  value of the target.
     *  
     *  @langversion 3.0
     *  @playerversion Flash 10
     *  @playerversion AIR 1.5
     *  @productversion Flex 4
     */
    public var xTo:Number;

    //----------------------------------
    //  yBy
    //----------------------------------

    [Inspectable(category="General", defaultValue="NaN")]

    /** 
     *  Number of pixels by which to modify the y position of the target.
     *  Values can be negative.
     *  
     *  @langversion 3.0
     *  @playerversion Flash 10
     *  @playerversion AIR 1.5
     *  @productversion Flex 4
     */
    public var yBy:Number;
    
    //----------------------------------
    //  yFrom
    //----------------------------------

    [Inspectable(category="General", defaultValue="NaN")]

    /** 
     *  Initial y position of the target, in pixels.
     *  If omitted, Flex uses either the value in the start view state,
     *  if the effect is playing in a transition, or the current
     *  value of the target.
     *  
     *  @langversion 3.0
     *  @playerversion Flash 10
     *  @playerversion AIR 1.5
     *  @productversion Flex 4
     */
    public var yFrom:Number;

    //----------------------------------
    //  yTo
    //----------------------------------

    [Inspectable(category="General", defaultValue="NaN")]

    /** 
     *  Final y position of the target, in pixels.
     *  If omitted, Flex uses either the value in the end view state,
     *  if the effect is playing in a transition, or the current
     *  value of the target.
     *  
     *  @langversion 3.0
     *  @playerversion Flash 10
     *  @playerversion AIR 1.5
     *  @productversion Flex 4
     */
    public var yTo:Number;
            
    //----------------------------------
    //  zBy
    //----------------------------------

    [Inspectable(category="General", defaultValue="NaN")]

    /** 
     *  Number of pixels by which to modify the z position of the target.
     *  Values may be negative.
     * 
     *  @langversion 3.0
     *  @playerversion Flash 10
     *  @playerversion AIR 1.5
     *  @productversion Flex 4
     */
    public var zBy:Number;
    
    //----------------------------------
    //  zFrom
    //----------------------------------

    [Inspectable(category="General", defaultValue="NaN")]

    /** 
     *  Initial z position of the target.
     *  If omitted, Flex uses either the value in the starting view state, 
     *  if the effect is playing in a transition, or the current value of the target.
     * 
     *  @langversion 3.0
     *  @playerversion Flash 10
     *  @playerversion AIR 1.5
     *  @productversion Flex 4
     */
    public var zFrom:Number;

    //----------------------------------
    //  zTo
    //----------------------------------

    [Inspectable(category="General", defaultValue="NaN")]

    /** 
     *  Final z position of the target.
     *  If omitted, Flex uses either the value in the starting state, 
     *  if the effect is playing in a state transition, or the current value of the target.
     * 
     *  @langversion 3.0
     *  @playerversion Flash 10
     *  @playerversion AIR 1.5
     *  @productversion Flex 4
     */
    public var zTo:Number;

    //--------------------------------------------------------------------------
    //
    //  Overridden methods
    //
    //--------------------------------------------------------------------------

    /**
     *  @private
     */
    override public function get relevantStyles():Array /* of String */
    {
        return RELEVANT_STYLES;
    }   

    /**
     *  @private
     */
    override public function getAffectedProperties():Array /* of String */
    {
        return AFFECTED_PROPERTIES;
    }

    // TODO (chaase): Should try to remove this override. At a minimum, we could
    // put the motionPaths creation at the start of initInstance(). Ideally, we'd
    // remove that logic entirely, but there's a need to create motionPaths fresh
    // for every call to create/initInstance() or else multi-instance effects
    // will inherit the one motionPaths object created elsewhere.
    /**
     * @private
     */
    override public function createInstance(target:Object = null):IEffectInstance
    {
        motionPaths = new Vector.<MotionPath>();
        return super.createInstance(target);
    }
                        
    /**
     * @private
     */
   override protected function initInstance(instance:IEffectInstance):void
    {        
        var xProp:String = applyChangesPostLayout ? "postLayoutTranslationX" : "translationX";
        var yProp:String = applyChangesPostLayout ? "postLayoutTranslationY" : "translationY";
        var zProp:String = applyChangesPostLayout ? "postLayoutTranslationZ" : "translationZ";
        
        addMotionPath(xProp, xFrom, xTo, xBy);
        addMotionPath(yProp, yFrom, yTo, yBy);
        addMotionPath(zProp, zFrom, zTo, zBy);
        
        super.initInstance(instance);
    }    
}
}
