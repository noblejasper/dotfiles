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

package mx.containers.utilityClasses
{

/**
 *  IConstraintLayout is a marker interface that indicates that a container
 *  supports ConstraintColumn class and ConstraintRow class within its layout. 
 *  Application, Canvas, and Panel containers support ConstraintRow and  
 *  ConstraintColumn classes.
 *  To utilize this type of constraint in these containers,
 *  set the <code>layout</code> property to <code>"absolute"</code>
 *  and create ConstraintColumn and ConstraintRow instances. 
 * 
 *  @see mx.containers.Canvas
 *  @see mx.containers.Panel
 *  @see mx.core.Application
 *  @see mx.containers.utilityClasses.ConstraintColumn
 *  @see mx.containers.utilityClasses.ConstraintRow
 *  @see mx.modules.Module
 *  
 *  @langversion 3.0
 *  @playerversion Flash 9
 *  @playerversion AIR 1.1
 *  @productversion Flex 3
 */
public interface IConstraintLayout
{
    //--------------------------------------------------------------------------
    //
    //  Properties
    //
    //--------------------------------------------------------------------------
    
    //----------------------------------
    //  constraintColumns
    //----------------------------------

    /**
     *  An Array of ConstraintColumn instances that partition this container.
     *  The ConstraintColumn instance at index 0 is the left-most column;
     *  indices increase from left to right. 
     * 
     *  @default []
     *  
     *  @langversion 3.0
     *  @playerversion Flash 9
     *  @playerversion AIR 1.1
     *  @productversion Flex 3
     */
    function get constraintColumns():Array /* of ConstraintColumn */;
    
    /**
     *  @private
     */
    function set constraintColumns(value:Array /* of ConstraintColumn */):void;
    
    //----------------------------------
    //  constraintRows
    //----------------------------------
    
    /**
     *  An Array of ConstraintRow instances that partition this container.
     *  The ConstraintRow instance at index 0 is the top-most row;
     *  indices increase from top to bottom.
     * 
     *  @default []
     *  
     *  @langversion 3.0
     *  @playerversion Flash 9
     *  @playerversion AIR 1.1
     *  @productversion Flex 3
     */
    function get constraintRows():Array /* of ConstraintRow */;
    
    /**
     *  @private
     */
    function set constraintRows(value:Array /* of ConstraintRow */):void;
}

}
