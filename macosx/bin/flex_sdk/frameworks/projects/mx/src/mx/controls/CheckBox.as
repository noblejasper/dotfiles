////////////////////////////////////////////////////////////////////////////////
//
//  ADOBE SYSTEMS INCORPORATED
//  Copyright 2003-2007 Adobe Systems Incorporated
//  All Rights Reserved.
//
//  NOTICE: Adobe permits you to use, modify, and distribute this file
//  in accordance with the terms of the license agreement accompanying it.
//
////////////////////////////////////////////////////////////////////////////////

package mx.controls
{

import mx.core.FlexVersion;
import mx.core.IToggleButton;
import mx.core.mx_internal;
import mx.core.UITextField;
import flash.text.TextLineMetrics;
import flash.utils.getQualifiedClassName;

use namespace mx_internal;

//--------------------------------------
//  Styles
//--------------------------------------

include "../styles/metadata/IconColorStyles.as"

/**
 *  Color of any symbol of a component. Examples include the check mark of a CheckBox or
 *  the arrow of a ScrollBar button.
 *   
 *  @default 0x000000
 * 
 *  @langversion 3.0
 *  @playerversion Flash 10
 *  @playerversion AIR 1.5
 *  @productversion Flex 4
 */ 
[Style(name="symbolColor", type="uint", format="Color", inherit="yes", theme="spark")]

//--------------------------------------
//  Excluded APIs
//--------------------------------------

[Exclude(name="emphasized", kind="property")]
[Exclude(name="toggle", kind="property")]

//--------------------------------------
//  Other metadata
//--------------------------------------

[AccessibilityClass(implementation="mx.accessibility.CheckBoxAccImpl")]

[DefaultBindingProperty(source="selected", destination="selected")]

[DefaultTriggerEvent("click")]

[IconFile("CheckBox.png")]

[Alternative(replacement="spark.components.CheckBox", since="4.0")]

/**
 *  The CheckBox control consists of an optional label and a small box
 *  that can contain a check mark or not. 
 *  You can place the optional text label to the left, right, top, or bottom
 *  of the CheckBox.
 *  When a user clicks a CheckBox control or its associated text,
 *  the CheckBox control changes its state
 *  from checked to unchecked or from unchecked to checked.
 *  CheckBox controls gather a set of true or false values
 *  that are not mutually exclusive.
 *
 *  <p>The CheckBox control has the following default characteristics:</p>
 *     <table class="innertable">
 *        <tr>
 *           <th>Characteristic</th>
 *           <th>Description</th>
 *        </tr>
 *        <tr>
 *           <td>Default size</td>
 *           <td>A size large enough to hold the label</td>
 *        </tr>
 *        <tr>
 *           <td>Minimum size</td>
 *           <td>0 pixels</td>
 *        </tr>
 *        <tr>
 *           <td>Maximum size</td>
 *           <td>No limit</td>
 *        </tr>
 *     </table>
 *
 *  @mxml
 *
 *  <p>The <code>&lt;mx:CheckBox&gt;</code> tag inherits all of the tag
 *  attributes of its superclass and adds the following tag attributes:</p>
 *  
 *  <pre>
 *  &lt;mx:CheckBox
 *    <strong>Styles</strong>
 *    disabledIconColor="0x999999"
 *    iconColor="0x2B333C"
 *  /&gt;
 *  </pre>
 *
 *  @includeExample examples/CheckBoxExample.mxml
 *  
 *  @langversion 3.0
 *  @playerversion Flash 9
 *  @playerversion AIR 1.1
 *  @productversion Flex 3
 */
public class CheckBox extends Button implements IToggleButton
{
    include "../core/Version.as";

    //--------------------------------------------------------------------------
    //
    //  Class mixins
    //
    //--------------------------------------------------------------------------

    /**
     *  @private
     *  Placeholder for mixin by CheckBoxAccImpl.
     */
    mx_internal static var createAccessibilityImplementation:Function;

    //--------------------------------------------------------------------------
    //
    //  Constructor
    //
    //--------------------------------------------------------------------------

    /**
     *  Constructor.
     *  
     *  @langversion 3.0
     *  @playerversion Flash 9
     *  @playerversion AIR 1.1
     *  @productversion Flex 3
     */
    public function CheckBox()
    {
        super();

        // Button variables.
        _toggle = true;
        
        // Used for the old padding logic
        centerContent = false;
        extraSpacing = 8;
    }

    //--------------------------------------------------------------------------
    //
    //  Overridden properties
    //
    //--------------------------------------------------------------------------

    //----------------------------------
    //  emphasized
    //----------------------------------

    [Inspectable(environment="none")]

    /**
     *  @private
     *  A CheckBox doesn't have an emphasized state, so _emphasized
     *  is set false in the constructor and can't be changed via this setter.
     */
    override public function set emphasized(value:Boolean):void
    {
    }

    //----------------------------------
    //  toggle
    //----------------------------------

    [Inspectable(environment="none")]

    /**
     *  @private
     *  A CheckBox is always toggleable by definition, so _toggle is set
     *  true in the constructor and can't be changed via this setter.
     */
    override public function set toggle(value:Boolean):void
    {
    }

    //--------------------------------------------------------------------------
    //
    //  Overridden methods: UIComponent
    //
    //--------------------------------------------------------------------------

    /**
     *  @private
     */
    override protected function initializeAccessibility():void
    {
        if (CheckBox.createAccessibilityImplementation != null)
            CheckBox.createAccessibilityImplementation(this);
    }
    
    /**
     *  @private
     *  Returns the height that will accomodate the text and icon.
     */
    override protected function measure():void
    {
        super.measure();

        if (!label &&
            FlexVersion.compatibilityVersion >= FlexVersion.VERSION_4_0 &&
            getQualifiedClassName(currentIcon).indexOf(".spark") >= 0)
        {
            // Adjust the height to account for text height, even when there
            // is no label. super.measure() handles non-null label case, so we just
            // handle null label case here
            var lineMetrics:TextLineMetrics = measureText(label);
            var textH:Number = lineMetrics.height + UITextField.TEXT_HEIGHT_PADDING;
            textH += getStyle("paddingTop") + getStyle("paddingBottom");
            measuredMinHeight = measuredHeight = Math.max(textH, measuredMinHeight);
        }
    }
}

}
