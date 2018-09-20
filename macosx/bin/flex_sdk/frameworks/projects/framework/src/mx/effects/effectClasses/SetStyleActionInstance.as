////////////////////////////////////////////////////////////////////////////////
//
//  ADOBE SYSTEMS INCORPORATED
//  Copyright 2005-2006 Adobe Systems Incorporated
//  All Rights Reserved.
//
//  NOTICE: Adobe permits you to use, modify, and distribute this file
//  in accordance with the terms of the license agreement accompanying it.
//
////////////////////////////////////////////////////////////////////////////////

package mx.effects.effectClasses
{

import mx.core.mx_internal;
import mx.styles.StyleManager;
import mx.core.IFlexModuleFactory;
import mx.core.IFlexModule;

use namespace mx_internal;

/**
 *  The SetStyleActionInstance class implements the instance class
 *  for the SetStyleAction effect.
 *  Flex creates an instance of this class when it plays a SetStyleAction
 *  effect; you do not create one yourself.
 *
 *  @see mx.effects.SetStyleAction
 *  
 *  @langversion 3.0
 *  @playerversion Flash 9
 *  @playerversion AIR 1.1
 *  @productversion Flex 3
 */  
public class SetStyleActionInstance extends ActionEffectInstance
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
	 *  @param target The Object to animate with this effect.
	 *  
	 *  @langversion 3.0
	 *  @playerversion Flash 9
	 *  @playerversion AIR 1.1
	 *  @productversion Flex 3
	 */
	public function SetStyleActionInstance(target:Object)
	{
		super(target);
	}

	//--------------------------------------------------------------------------
	//
	//  Properties
	//
	//--------------------------------------------------------------------------
	
	//----------------------------------
	//  name
	//----------------------------------

	/** 
	 *  The name of the style property being changed.
	 *  
	 *  @langversion 3.0
	 *  @playerversion Flash 9
	 *  @playerversion AIR 1.1
	 *  @productversion Flex 3
	 */
	public var name:String;
	
	//----------------------------------
	//  value
	//----------------------------------

	/** 
	 *  @private
	 *  Storage for the value property.
	 */
	private var _value:*;
	
	/** 
	 *  The new value for the property.
	 *  
	 *  @langversion 3.0
	 *  @playerversion Flash 9
	 *  @playerversion AIR 1.1
	 *  @productversion Flex 3
	 */
	public function get value():*
	{
		if (playReversed)
			return getStartValue();
		else
			return _value;
	}
	
	/** 
	 *  @private
	 */
	public function set value(val:*):void
	{
		_value = val;
	}
	
	//--------------------------------------------------------------------------
	//
	//  Overridden methods
	//
	//--------------------------------------------------------------------------
		
	/**
	 *  @private
	 */
	override public function play():void
	{
		// Dispatch an effectStart event from the target.
		super.play();	
		
		if (value === undefined && propertyChanges)
		{
			if (name in propertyChanges.end &&
				propertyChanges.start[name] != propertyChanges.end[name])
				value = propertyChanges.end[name];
		}
		
		// Set the style property
		if (target && name && value !== undefined)
		{
			var currentValue:Object = target.getStyle(name);
			
			if (currentValue is Number)
			{
				// The "value" for colors can be several different formats:
				// 0xNNNNNN, #NNNNNN or "red". We can't use
				// StyleManager.isColorStyle() because that only returns true
				// for inheriting color styles and misses non-inheriting styles like
				// backgroundColor.
				if (name.toLowerCase().indexOf("color") != -1)
                {
                    var moduleFactory:IFlexModuleFactory = null;
                    if (target is IFlexModule)
                        moduleFactory = target.moduleFactory;
                    target.setStyle(name, 
                        StyleManager.getStyleManager(moduleFactory).getColorName(value));                    
                }
				else
					target.setStyle(name, Number(value));
			}
			else if (currentValue is Boolean)
			{
				if (value is String)
					target.setStyle(name, (value.toLowerCase() == "true"));
				else
					target.setStyle(name, value);
			}
			else
			{
				target.setStyle(name, value);
			}
		}
		
		// We're done...
		finishRepeat();
	}
	
	/** 
	 *  @private
	 */
	override protected function saveStartValue():*
	{
		return target.getStyle(name);
	}
}

}
