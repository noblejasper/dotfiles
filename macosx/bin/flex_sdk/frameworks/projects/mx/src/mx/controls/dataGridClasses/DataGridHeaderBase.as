////////////////////////////////////////////////////////////////////////////////
//
//  ADOBE SYSTEMS INCORPORATED
//  Copyright 2007 Adobe Systems Incorporated
//  All Rights Reserved.
//
//  NOTICE: Adobe permits you to use, modify, and distribute this file
//  in accordance with the terms of the license agreement accompanying it.
//
////////////////////////////////////////////////////////////////////////////////

package mx.controls.dataGridClasses
{

import flash.display.DisplayObject;
import flash.display.GradientType;
import flash.display.Graphics;
import flash.display.Shape;
import flash.display.Sprite;
import flash.events.MouseEvent;
import flash.geom.Matrix;
import flash.geom.Point;
import flash.geom.Rectangle;
import mx.controls.listClasses.IDropInListItemRenderer;
import mx.controls.listClasses.IListItemRenderer;
import mx.controls.DataGrid;
import mx.core.EdgeMetrics;
import mx.core.FlexSprite;
import mx.core.IFlexDisplayObject;
import mx.core.UIComponent;
import mx.core.UIComponentGlobals;
import mx.events.DataGridEvent;
import mx.managers.CursorManager;
import mx.managers.CursorManagerPriority;
import mx.styles.ISimpleStyleClient;
import mx.core.mx_internal;
import mx.effects.easing.Back;

use namespace mx_internal;

/**
 *  The DataGridHeaderBase class defines the base class for the DataGridHeader class,
 *  the class that defines the item renderer for the DataGrid control. 
 *  
 *  @langversion 3.0
 *  @playerversion Flash 9
 *  @playerversion AIR 1.1
 *  @productversion Flex 3
 */
public class DataGridHeaderBase extends UIComponent
{

    /**
     *  Constructor. 
     *  
     *  @langversion 3.0
     *  @playerversion Flash 9
     *  @playerversion AIR 1.1
     *  @productversion Flex 3
     */
    public function DataGridHeaderBase()
    {
        super();
    }

    /**
     *  a layer to draw selections
     *  
     *  @langversion 3.0
     *  @playerversion Flash 9
     *  @playerversion AIR 1.1
     *  @productversion Flex 3
     */
    mx_internal var selectionLayer:Sprite;

    /**
     *  a function to clear selections
     *  
     *  @langversion 3.0
     *  @playerversion Flash 9
     *  @playerversion AIR 1.1
     *  @productversion Flex 3
     */
    mx_internal function clearSelectionLayer():void
    {
    }

   /**
     *  @private
     *  the set of columns for this header
     */
    mx_internal var visibleColumns:Array;

   /**
     *  @private
     *  the set of columns for this header
     */
    mx_internal var headerItemsChanged:Boolean = false;
}

}