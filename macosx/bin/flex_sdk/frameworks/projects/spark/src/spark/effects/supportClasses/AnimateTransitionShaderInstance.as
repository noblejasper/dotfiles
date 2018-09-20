////////////////////////////////////////////////////////////////////////////////
//
//  ADOBE SYSTEMS INCORPORATED
//  Copyright 2008 Adobe Systems Incorporated
//  All Rights Reserved.
//
//  NOTICE: Adobe permits you to use, modify, and distribute this file
//  in accordance with the terms of the license agreement accompanying it.
//
////////////////////////////////////////////////////////////////////////////////
package spark.effects.supportClasses
{
import flash.display.BitmapData;
import flash.display.Shader;
import flash.filters.ShaderFilter;
import flash.geom.Point;
import flash.geom.Rectangle;
import flash.utils.ByteArray;

import mx.core.IUIComponent;
import mx.core.mx_internal;
import mx.resources.IResourceManager;
import mx.resources.ResourceManager;

import spark.effects.animation.Animation;
import spark.effects.animation.Keyframe;
import spark.effects.animation.MotionPath;
import spark.effects.animation.SimpleMotionPath;
import spark.primitives.supportClasses.GraphicElement;
import spark.utils.BitmapUtil;
    
use namespace mx_internal;    
    
/**
 *  The AnimateTransitionShaderInstance class implements the instance class for the
 *  AnimateTransitionShader effect. Flex creates an instance of this class when
 *  it plays a AnimateTransitionShader effect; you do not create one yourself.
 *
 *  @see spark.effects.AnimateTransitionShader
 *  
 *  @langversion 3.0
 *  @playerversion Flash 10
 *  @playerversion AIR 1.5
 *  @productversion Flex 4
 */
public class AnimateTransitionShaderInstance extends AnimateInstance
{
    include "../../core/Version.as";

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
    public function AnimateTransitionShaderInstance(target:Object)
    {
        super(target);
        // Automatically keep disappearing targets around during this effect
        autoRemoveTarget = true;
    }

    //--------------------------------------------------------------------------
    //
    //  Class variables
    //
    //--------------------------------------------------------------------------
    
    /**
     *  @private
     *  Storage for the resourceManager getter.
     *  This gets initialized on first access,
     *  not at static initialization time, in order to ensure
     *  that the Singleton registry has already been initialized.
     */
    private static var _resourceManager:IResourceManager;
    
    /**
     *  @private
     *  A reference to the object which manages
     *  all of the application's localized resources.
     *  This is a singleton instance which implements
     *  the IResourceManager interface.
     */
    private static function get resourceManager():IResourceManager
    {
        if (!_resourceManager)
            _resourceManager = ResourceManager.getInstance();

        return _resourceManager;
    }

    /**
     * @private
     * We need to dispose any BitmapData objects that we create in the
     * process of playing this effect. If the caller passes in the objects,
     * we'll let them handle disposal. These variables track whether we create
     * the bitmapFrom/bitmapTo objects, and therefore whether we need to
     * dispose of them when we're finished.
     */
    private var disposeFrom:Boolean;
    private var disposeTo:Boolean;
    
    //--------------------------------------------------------------------------
    //
    //  Properties
    //
    //--------------------------------------------------------------------------

    /**
     *  @copy spark.effects.AnimateTransitionShader#bitmapFrom
     *  
     *  @langversion 3.0
     *  @playerversion Flash 10
     *  @playerversion AIR 1.5
     *  @productversion Flex 4
     */
    public var bitmapFrom:BitmapData;
    
    /**
     *  @copy spark.effects.AnimateTransitionShader#bitmapTo
     *  
     *  @langversion 3.0
     *  @playerversion Flash 10
     *  @playerversion AIR 1.5
     *  @productversion Flex 4
     */
    public var bitmapTo:BitmapData;

    /**
     *  @copy spark.effects.AnimateTransitionShader#shaderByteCode
     *  
     *  @langversion 3.0
     *  @playerversion Flash 10
     *  @playerversion AIR 1.5
     *  @productversion Flex 4
     */
    public var shaderByteCode:ByteArray;
    
    /**
     *  @copy spark.effects.AnimateTransitionShader#shaderProperties
     *  
     *  @langversion 3.0
     *  @playerversion Flash 10
     *  @playerversion AIR 1.5
     *  @productversion Flex 4
     */
    public var shaderProperties:Object;

    /**
     * The Shader that is created using the <code>shaderByteCode</code>
     * property as the underlying byte code. Each instance needs its
     * own separate Shader, but can share the byte code. When each instance
     * is played, create the Shader that the instance uses.
     *  
     *  @langversion 3.0
     *  @playerversion Flash 10
     *  @playerversion AIR 1.5
     *  @productversion Flex 4
     */
    protected var shader:Shader;    
    
    /**
     *  @private
     *  Tracks whether we have bitmaps. When we don't, we
     *  don't create the shader and let the effect run.
     */
    private var hasBitmaps:Boolean = true;
        
    //--------------------------------------------------------------------------
    //
    //  Overridden methods
    //
    //--------------------------------------------------------------------------

    /**
     * The filter wrapped around the instance's <code>shader</code>
     * property. This filter is assigned to the <code>filters</code>
     * property of the target object with every update during the animation,
     * so that animated updates to the underlying shader are reflected
     * in the filter applied to the display object that the user sees.
     *  
     *  @langversion 3.0
     *  @playerversion Flash 10
     *  @playerversion AIR 1.5
     *  @productversion Flex 4
     */
    protected var shaderFilter:ShaderFilter;
    
    /**
     *  @private
     */
    override public function play():void
    {  
        // Only dispose bitmaps that are not provided by the caller
        disposeFrom = (bitmapFrom == null);
        disposeTo = (bitmapTo == null);
        
        // If we got the bitmaps from the state information in a transition, they
        // are stored along with the visual bounds of the object in an Object
        var boundsFrom:Rectangle;
        var boundsTo:Rectangle;
        var bmHolder:Object;
        
        // TODO (chaase): Should take the 'from' snapshot on the
        // fly, in case the object has changed since the overall
        // effect (composite, etc) started much earlier and the
        // object has changed since propertyChanges was initialized 
        if (!bitmapFrom && propertyChanges &&
            propertyChanges.start["bitmapInfo"] !== undefined)
        {
            bmHolder = propertyChanges.start["bitmapInfo"];
            if (bmHolder)
            {
                bitmapFrom = bmHolder["bitmap"];
                boundsFrom = bmHolder["bounds"];
            }
        }
        if (!bitmapTo && propertyChanges &&
            propertyChanges.end["bitmapInfo"] !== undefined)
        {
            bmHolder = propertyChanges.end["bitmapInfo"];
            if (bmHolder)
            {
                bitmapTo = bmHolder["bitmap"];
                boundsTo = bmHolder["bounds"];
            }
        }
        if (!bitmapFrom)
            if (propertyChanges &&
                (propertyChanges.start["visible"] == false ||
                 propertyChanges.start["parent"] == null))
                if (bitmapTo)                    
                    bitmapFrom = new BitmapData(bitmapTo.width, bitmapTo.height, true, 0);
        if (!bitmapTo)
            if (propertyChanges &&
                (propertyChanges.end["visible"] == false ||
                 propertyChanges.end["parent"] == null))
                if (bitmapFrom)                    
                    bitmapTo = new BitmapData(bitmapFrom.width, bitmapFrom.height, true, 0);
        
        // Last-ditch effort - if we don't have bitmaps yet, then just grab a 
        // snapshot of the current target
        if (!bitmapFrom)
            bitmapFrom = getSnapshot(target);
        if (!bitmapTo)
            bitmapTo = getSnapshot(target);
        
        // When all efforts to get a bitmap fail, we don't create a shader.
        if (!bitmapFrom || !bitmapTo)
        {
            hasBitmaps = false;
            
            // clean up
            if (bitmapFrom)
                bitmapFrom.dispose();
            else if (bitmapTo)
                bitmapTo.dispose();
        }
        else
        {
            hasBitmaps = true;
        }

        // Fix up the visibility if it's becoming visible
        if (propertyChanges &&
            !propertyChanges.start["visible"] &&
            propertyChanges.end["visible"])
        {
            target.visible = true;
        }
        
        // Only create shader if bitmaps are present.
        if (hasBitmaps)
        {
            shader = new Shader(shaderByteCode);
            if (shader.data)
            {
                if (boundsFrom && boundsTo && 
                    (boundsFrom.x != boundsTo.x || boundsFrom.y != boundsTo.y))
                {
                    var newX:Number = (boundsTo.x - boundsFrom.x);
                    var newY:Number = (boundsTo.y - boundsFrom.y);
                    var newW:Number = boundsFrom.width - newX;
                    var newH:Number = boundsFrom.height - newY;
                    var newBitmapFrom:BitmapData = new BitmapData(newW, newH, true, 0);
                    newBitmapFrom.copyPixels(bitmapFrom, 
                        new Rectangle(newX, newY, newW, newH),
                        new Point(0, 0));
                    if (disposeFrom)
                        bitmapFrom.dispose();
                    bitmapFrom = newBitmapFrom;
                }
                shader.data.from.input = bitmapFrom;
                shader.data.to.input = bitmapTo;
                
                motionPaths = new <MotionPath>[new MotionPath("progress")];
                motionPaths[0].keyframes = new <Keyframe>[new Keyframe(0, 0), 
                    new Keyframe(duration, 1)];
                
                // auto-set width/height if exposed in shader
                if ("width" in shader.data)
                    shader.data.width.value = [Math.max(bitmapFrom.width, bitmapTo.width)];
                if ("height" in shader.data)
                    shader.data.height.value = [Math.max(bitmapFrom.height,bitmapTo.height)];
                if (shaderProperties)
                {
                    for (var prop:String in shaderProperties)
                    {
                        var value:Object = shaderProperties[prop];
                        shader.data[prop].value = (value is Array) ?
                            value :
                            [value];
                    }
                }
                shaderFilter = new ShaderFilter(shader);
            }
        }
        
        super.play();
    }    
    
    /**
     * @private
     * Return a BitmapData for the target object
     */
    private function getSnapshot(target:Object):BitmapData
    {
        if (!(target is GraphicElement || target is IUIComponent))
            throw new Error(resourceManager.getString("sparkEffects", "cannotOperateOn"));
        var bmData:BitmapData;
        var tempFilters:Array = target.filters;
        target.filters = [];
        
        try
        {
            if (target is GraphicElement)
                bmData = GraphicElement(target).captureBitmapData(true, 0, false);
            else
                bmData = BitmapUtil.getSnapshot(IUIComponent(target), null, true);
        }
        catch (e:SecurityError)
        {
            // Do nothing and let it return null.
        }
        
        target.filters = tempFilters;
        
        return bmData;
    }
    /**
     * Unlike Animate's setValue we assign the new value to the filter
     * associated with our effect instance rather than the target of 
     * the effect. 
     *  
     * @private
     */
    override protected function setValue(property:String, value:Object):void
    {
        if (!hasBitmaps)
            return;
        
        shader.data.progress.value = [value];
        target.filters = [shaderFilter].concat(acquireFilters());
    }

    /**
     * @private
     */
    override public function animationStart(animation:Animation):void
    {
        super.animationStart(animation);
        
        if (!hasBitmaps)
            return;
        
        target.filters = [shaderFilter].concat(target.filters);;
    }

    /**
     * @private
     * Called when animation ends. Restores original filters and disposes of
     * any BitmapData objects we created.
     */
    private function cleanup():void
    {
        if (!hasBitmaps)
            return;
        
        target.filters = acquireFilters();
        
        if (disposeFrom)
            bitmapFrom.dispose();
        if (disposeTo)
            bitmapTo.dispose();
    }
    
    /**
     * Acquires the current list of filters for our target, *excluding*
     * our active shader filter. 
     * @private
     */
    private function acquireFilters():Array
    {
        var filters:Array = target.filters;
        for (var i:int=0; i < filters.length; i++)
        {
            if (filters[i] == shaderFilter)
            {
                filters.splice(i, 1);
                break;
            }
        }
        return filters;
    }
    
    /**
     * @private
     */
    override public function animationEnd(animation:Animation):void
    {
        cleanup();
        super.animationEnd(animation);
    }

    /**
     * @private
     */
    override public function animationStop(animation:Animation):void
    {
        cleanup();
        super.animationStop(animation);
    }

    /**
     * Unlike Animate's getValue we return the value of the property requested
     * from the filter associated with our effect instance rather than 
     * the effect target.
     *  
     * @private
     */
    override protected function getCurrentValue(property:String):*
    {
        // null value may cause motion path interpolation to fail,
        // but this method generally won't be called for shader effects.
        if (!hasBitmaps)
            return null;
        
        return shader[property];
    }

    /**
     * Override Animate's setupStyleMapEntry to avoid the need to 
     * validate our properties against the 'target' (since we actually
     * set properties on our associated filter instance).
     *  
     * @private
     */
    override protected function setupStyleMapEntry(property:String):void
    {
    }
}
}
