////////////////////////////////////////////////////////////////////////////////
//
//  ADOBE SYSTEMS INCORPORATED
//  Copyright 2010 Adobe Systems Incorporated
//  All Rights Reserved.
//
//  NOTICE: Adobe permits you to use, modify, and distribute this file
//  in accordance with the terms of the license agreement accompanying it.
//
////////////////////////////////////////////////////////////////////////////////

package spark.events
{

import flash.events.Event;

import spark.components.DataGrid;

/**
 *  The GridSortEvent class represents events that are dispatched when
 *  the data provider of a Spark DataGrid control is sorted as the
 *  result of the user clicking on the header of a column in the DataGrid.
 * 
 *  @see spark.components.DataGrid
 *  @see spark.components.gridClasses.GridColumn
 *  @see spark.components.GridColumnHeaderGroup
 *  
 *  @langversion 3.0
 *  @playerversion Flash 10
 *  @playerversion AIR 2.5
 *  @productversion Flex 4.5
 */
public class GridSortEvent extends Event
{
    include "../core/Version.as";

    //--------------------------------------------------------------------------
    //
    //  Class constants
    //
    //--------------------------------------------------------------------------

    /**
     *  The <code>GridSortEvent.SORT_CHANGE</code> constant defines 
     *  the value of the <code>type</code> property of the event object for a 
     *  <code>sortChange</code> event, which indicates that a sort filter has just been
     *  applied to the grid's <code>dataProvider</code> collection.
     *
     *  <p>Typically, if a column header mouse click triggered the sort, then the last index of 
     *  <code>columnIndices</code> is the column's index.  
     *  Note that interactive sorts are not necessarily triggered by a mouse click.</p>
     * 
     *  <p>This event is dispatched when the user interacts with the control.
     *  When you sort the data provider's collection programmatically, 
     *  the component does not dispatch the <code>sortChange</code> event. </p>
     *
     *  <p>The properties of the event object have the following values:</p>
     *  <table class="innertable">
     *     <tr><th>Property</th><th>Value</th></tr>
     *     <tr><td><code>bubbles</code></td><td>false</td></tr>
     *     <tr><td><code>cancelable</code></td><td>false</td></tr>
     *     <tr><td><code>columnIndices</code></td><td>The vector of column indices of the 
     *       sorted columns.</td></tr>
     *     <tr><td><code>currentTarget</code></td><td>The Object that defines the 
     *       event listener that handles the event. For example, if you use 
     *       <code>myDataGrid.addEventListener()</code> to register an event listener, 
     *       myDataGrid is the value of the <code>currentTarget</code>. </td></tr>
     *     <tr><td><code>newSortFields</code></td><td>The array of ISortFields for this 
     *       sort.</td></tr>
     *     <tr><td><code>oldSortFields</code></td><td>The array of ISortFields for the 
     *       last sort.</td></tr>
     *     <tr><td><code>target</code></td><td>The Object that dispatched the event; 
     *       it is not always the Object listening for the event. 
     *       Use the <code>currentTarget</code> property to always access the 
     *       Object listening for the event.</td></tr>
     *     <tr><td><code>type</code></td><td>GridSelectionEvent.SORT_CHANGE</td></tr>
     *  </table>
     *   
     *  @eventType sortChange
     *  
     *  @see spark.components.DataGrid#dataProvider
     *  @see spark.components.DataGrid#sortByColumns
     * 
     *  @langversion 3.0
     *  @playerversion Flash 10
     *  @playerversion AIR 2.5
     *  @productversion Flex 4.5
     */
    public static const SORT_CHANGE:String = "sortChange";
    
    /**
     *  The <code>GridSortEvent.SORT_CHANGING</code> constant defines 
     *  the value of the <code>type</code> property of the event object for a 
     *  <code>sortChanging</code> event, which indicates that a sort filter is about to be
     *  applied to the grid's <code>dataProvider</code> collection.
     *  Call preventDefault() on this event to prevent the sort from occurring
     *  or you modify <code>columnIndices</code> and <code>newSortFields</code> if you want to 
     *  change the default behavior of the sort.
     *
     *  <p>Typically, if a column header mouse click triggered the sort, then the last index of 
     *  <code>columnIndices</code> is the column's index.  
     *  Note that interactive sorts are not necessarily triggered by a mouse click.</p>
     * 
     *  <p>This event is dispatched when the user interacts with the control.
     *  When you sort the data provider's collection programmatically, 
     *  the component does not dispatch the <code>sortChanging</code> event. </p>
     *
     *  <p>The properties of the event object have the following values:</p>
     *  <table class="innertable">
     *     <tr><th>Property</th><th>Value</th></tr>
     *     <tr><td><code>bubbles</code></td><td>false</td></tr>
     *     <tr><td><code>cancelable</code></td><td>false</td></tr>
     *     <tr><td><code>columnIndices</code></td><td>The vector of column indices of the 
     *       columns to be sorted.</td></tr>
     *     <tr><td><code>currentTarget</code></td><td>The Object that defines the 
     *       event listener that handles the event. For example, if you use 
     *       <code>myDataGrid.addEventListener()</code> to register an event listener, 
     *       myDataGrid is the value of the <code>currentTarget</code>. </td></tr>
     *     <tr><td><code>newSortFields</code></td><td>The array of ISortFields for this 
     *       sort.</td></tr>
     *     <tr><td><code>oldSortFields</code></td><td>The array of ISortFields for the 
     *       last sort.</td></tr>
     *     <tr><td><code>target</code></td><td>The Object that dispatched the event; 
     *       it is not always the Object listening for the event. 
     *       Use the <code>currentTarget</code> property to always access the 
     *       Object listening for the event.</td></tr>
     *     <tr><td><code>type</code></td><td>GridSelectionEvent.SORT_CHANGING</td></tr>
     *  </table>
     *   
     *  @eventType sortChanging
     *  
     *  @see spark.components.DataGrid#dataProvider
     *  @see spark.components.DataGrid#sortByColumns
     * 
     *  @langversion 3.0
     *  @playerversion Flash 10
     *  @playerversion AIR 2.5
     *  @productversion Flex 4.5
     */
    public static const SORT_CHANGING:String = "sortChanging";
        
    //--------------------------------------------------------------------------
    //
    //  Constructor
    //
    //--------------------------------------------------------------------------

    /**
     *  Constructor.
     *
     *  @param type The event type; indicates the action that caused the event.
     *
     *  @param bubbles Specifies whether the event can bubble
     *  up the display list hierarchy.
     *
     *  @param cancelable Specifies whether the behavior
     *  associated with the event can be prevented.
     *
     *  @param columnIndices The vector of column indices of the sorted columns.
     * 
     *  @param oldSortFields The array of ISortFields for the last sort.
     * 
     *  @param newSortFields The array of ISortFields for this sort.
     * 
     *  @langversion 3.0
     *  @playerversion Flash 10
     *  @playerversion AIR 2.5
     *  @productversion Flex 4.5
     */    
    public function GridSortEvent(type:String, 
                                  bubbles:Boolean,
                                  cancelable:Boolean,
                                  columnIndices:Vector.<int>,
                                  oldSortFields:Array, /* ISortField */
                                  newSortFields:Array) /* ISortField */ 
     {
        super(type, bubbles, cancelable);

        this.columnIndices = columnIndices ? columnIndices.concat() : null;
        
        // These are shallow copies.
        _oldSortFields = oldSortFields ? oldSortFields.concat() : null;
        this.newSortFields = newSortFields ? newSortFields.concat() : null;
    }

    //--------------------------------------------------------------------------
    //
    // Properties
    //
    //--------------------------------------------------------------------------
    
    //----------------------------------
    //  columnIndices
    //----------------------------------
    
    /**
     *  The vector of column indices of the sorted columns.  
     *  If <code>type</code> is <code>GridSelectionEvent.SORT_CHANGING</code> this value
     *  can be modified and it will be used to update the grid's <code>columnHeaderGroup</code>
     *  <code>visibleSortIndicatorIndices</code>.
     *
     *  @see spark.components.DataGrid#columnHeaderGroup
     *  @see spark.components.GridColumnHeaderGroup#visibleSortIndicatorIndices
     *  @see spark.components.gridClasses.GridColumn#columnIndex
     * 
     *  @langversion 3.0
     *  @playerversion Flash 10
     *  @playerversion AIR 2.5
     *  @productversion Flex 4.5
     */
    public var columnIndices:Vector.<int>;
        
    //----------------------------------
    //  newSortFields
    //----------------------------------

    /**
     *  The array of ISortFields for this sort.
     *  If <code>type</code> is <code>GridSelectionEvent.SORT_CHANGING</code> this value
     *  can be modified and it will be used to sort the <code>dataProvider</code> of the grid.
     *
     *  @see spark.components.DataGrid#dataProvider
     *  @see spark.components.gridClasses.GridColumn#sortCompareFunction
     *  @see spark.components.gridClasses.GridColumn#sortDescending
     *  @see spark.components.gridClasses.GridColumn#sortField
     * 
     *  @langversion 3.0
     *  @playerversion Flash 10
     *  @playerversion AIR 2.5
     *  @productversion Flex 4.5
     */
    public var newSortFields:Array;  /* of ISortField */
    
    //----------------------------------
    //  oldSortFields
    //----------------------------------
    
    private var _oldSortFields:Array;  /* of ISortField */ 
    
    /**
     *  The array of ISortFields for the last sort.  This can be <code>null</code>.
     *  The elements in this Array should not be modified.  
     *
     *  @see spark.components.DataGrid#dataProvider
     *  @see spark.components.gridClasses.GridColumn#sortCompareFunction
     *  @see spark.components.gridClasses.GridColumn#sortDescending
     *  @see spark.components.gridClasses.GridColumn#sortField
     * 
     *  @langversion 3.0
     *  @playerversion Flash 10
     *  @playerversion AIR 2.5
     *  @productversion Flex 4.5
     */
    public function get oldSortFields():Array
    {
        return _oldSortFields;
    }
    
    //--------------------------------------------------------------------------
    //
    //  Overridden methods: Object
    //
    //--------------------------------------------------------------------------
    
    /**
     *  @private
     */
    override public function toString():String
    {
        return formatToString(
            "GridSortEvent", "type", 
            "bubbles", "cancelable", "eventPhase",
            "columnIndices");
    }
    
    //--------------------------------------------------------------------------
    //
    //  Overridden methods: Event
    //
    //--------------------------------------------------------------------------

    /**
     *  @private
     */
    override public function clone():Event
    {
        return new GridSortEvent(
            type, bubbles, cancelable, 
            columnIndices, oldSortFields, newSortFields);
    }
}

}
