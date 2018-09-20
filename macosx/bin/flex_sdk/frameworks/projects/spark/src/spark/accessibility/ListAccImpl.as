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

package spark.accessibility
{

import spark.components.List;
import mx.core.UIComponent;
import mx.core.mx_internal;

use namespace mx_internal;

/**
 *  ListAccImpl is the accessibility implementation class
 *  for spark.components.List.
 *
 *  <p>When a Spark ListBase is created,
 *  its <code>accessibilityImplementation</code> property
 *  is set to an instance of this class.
 *  The Flash Player then uses this class to allow MSAA clients
 *  such as screen readers to see and manipulate the List.
 *  See the mx.accessibility.AccImpl and
 *  flash.accessibility.AccessibilityImplementation classes
 *  for background information about accessibility implementation
 *  classes and MSAA.</p>
 *
 *  <p><b>Children</b></p>
 *
 *  <p>The MSAA children of a list are its list items.
 *  The number of children is the number of items
 *  in the <code>dataProvider</code>,
 *  not just the number of visible renderers.</p>
 *
 *  <p>As described above, the accessibility of the list items
 *  is managed by the List; the <code>accessibilityImplementation</code>
 *  and <code>accessibilityProperties</code> of the item renderers
 *  are ignored by the Flash Player.</p>
 *
 *  <p><b>Role</b></p>
 *
 *  <p>The MSAA Role of a List is ROLE_SYSTEM_LIST.</p>
 *
 *  <p>The Role of each list item in the List is ROLE_SYSTEM_LISTITEM.</p>
 *
 *  <p><b>Name</b></p>
 *
 *  <p>The MSAA Name of a List is, by default, an empty string.
 *  When wrapped in a FormItem element, the Name is the FormItem's label.
 *  To override this behavior,
 *  set the List's <code>accessibilityName</code> property.</p>
 *
 *  <p>The Name of each list item is determined by the List's
 *  <code>itemToLabel()</code> method.</p>
 *
 *  <p>When the Name of the List or one of its items changes,
 *  a List dispatches the MSAA event EVENT_OBJECT_NAMECHANGE
 *  with the proper childID for a list item or 0 for itself.</p>
 *
 *  <p><b>Description</b></p>
 *
 *  <p>The MSAA Description of a List is, by default, an empty string,
 *  but you can set the List's <code>accessibilityDescription</code>
 *  property.</p>
 *
 *  <p>The Description of each list item is the empty string.</p>
 *
 *  <p><b>State</b></p>
 *
 *  <p>The MSAA State of a List is a combination of:
 *  <ul>
 *    <li>STATE_SYSTEM_UNAVAILABLE (when enabled is false)</li>
 *    <li>STATE_SYSTEM_FOCUSABLE (when enabled is true)</li>
 *    <li>STATE_SYSTEM_FOCUSED
 *    (when enabled is true and the List has focus)</li>
 *    <li>STATE_SYSTEM_MULTISELECTABLE
 *    (when allowMultipleSelection is true)</li>
 *  </ul></p>
 *
 *  <p>The State of a list item is a combination of:
 *  <ul>
 *    <li>STATE_SYSTEM_FOCUSABLE</li>
 *    <li>STATE_SYSTEM_FOCUSED (when focused)</li>
 *    <li>STATE_SYSTEM_SELECTABLE</li>
 *    <li>STATE_SYSTEM_SELECTED (when it has the caret)</li>
 *  </ul></p>
 *
 *  <p>When the State of the List or one of its items changes,
 *  a List dispatches the MSAA event EVENT_OBJECT_STATECHANGE
 *  with the proper childID for the list item or 0 for itself.</p>
 *
 *  <p><b>Value</b></p>
 *
 *  <p>A List or list item does not have an MSAA Value.</p>
 *
 *  <p><b>Location</b></p>
 *
 *  <p>The MSAA Location of a List or a list item is its bounding rectangle.</p>
 *
 *  <p><b>Default Action</b></p>
 *
 *  <p>A List does not have an MSAA DefaultAction.</p>
 *
 *  <p>The MSAA DefaultAction of a list item is "Double Click".</p>
 *
 *  <p>Performing the default action on a list item
 *  will cause it to be selected.</p>
 *
 *  <p><b>Focus</b></p>
 *
 *  <p>A List accepts focus.
 *  When it does so, it dispatches the MSAA event EVENT_OBJECT_FOCUS.</p>
 *
 *  <p><b>Selection</b></p>
 *
 *  <p>A List allows either a single or multiple list items to be selected,
 *  depending on the <code>allowMultipleSelection</code> property.
 *  When an item is selected,
 *  it dispatches MSAA event EVENT_OBJECT_SELECTION.</p>
 *  
 *  @langversion 3.0
 *  @playerversion Flash 10
 *  @playerversion AIR 1.5
 *  @productversion Flex 4
 */
public class ListAccImpl extends ListBaseAccImpl
{
    include "../core/Version.as";
    
    //--------------------------------------------------------------------------
    //
    //  Class methods
    //
    //--------------------------------------------------------------------------
    
    /**
     *  Enables accessibility in the List class.
     * 
     *  <p>This method is called by application startup code
     *  that is autogenerated by the MXML compiler.
     *  Afterwards, when instances of List are initialized,
     *  their <code>accessibilityImplementation</code> property
     *  will be set to an instance of this class.</p>
     *  
     *  @langversion 3.0
     *  @playerversion Flash 10
     *  @playerversion AIR 1.5
     *  @productversion Flex 4
     */
    public static function enableAccessibility():void
    {
        List.createAccessibilityImplementation =
            createAccessibilityImplementation;
    }
    
    /**
     *  @private
     *  Creates a List's AccessibilityImplementation object.
     *  This method is called from UIComponent's
     *  initializeAccessibility() method.
     */
    mx_internal static function createAccessibilityImplementation(
        component:UIComponent):void
    {
        component.accessibilityImplementation =
            new ListAccImpl(component);
    }
    
    //--------------------------------------------------------------------------
    //
    //  Constructor
    //
    //--------------------------------------------------------------------------
    
    /**
     *  Constructor.
     *
     *  @param master The UIComponent instance that this AccImpl instance
     *  is making accessible.
     *  
     *  @langversion 3.0
     *  @playerversion Flash 10
     *  @playerversion AIR 1.5
     *  @productversion Flex 4
     */
    public function ListAccImpl(master:UIComponent)
    {
        super(master);
    }
}

}
