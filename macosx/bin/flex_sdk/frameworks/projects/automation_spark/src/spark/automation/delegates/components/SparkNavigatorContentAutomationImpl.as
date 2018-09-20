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

package spark.automation.delegates.components
{
    import flash.display.DisplayObject;
    
    import mx.automation.Automation;
    import mx.core.mx_internal;
    
    import spark.components.NavigatorContent;
    
    
    use namespace mx_internal;
    
    [Mixin]
    
    /**
     * 
     *  Defines methods and properties required to perform instrumentation for the 
     *  NavigatorContent control.
     * 
     *  @see spark.components.NavigatorContent 
     *  
     *  @langversion 3.0
     *  @playerversion Flash 9
     *  @playerversion AIR 1.5
     *  @productversion Flex 4
     */
    public class SparkNavigatorContentAutomationImpl extends SparkSkinnableContainerAutomationImpl
    {
        include "../../../core/Version.as";
        
        //--------------------------------------------------------------------------
        //
        //  Class methods
        //
        //--------------------------------------------------------------------------
        
        /**
         *  Registers the delegate class for a component class with automation manager.
         *  
         *  @param root The SystemManger of the application.
         *  
         *  @langversion 3.0
         *  @playerversion Flash 9
         *  @playerversion AIR 1.5
         *  @productversion Flex 4
         */
        public static function init(root:DisplayObject):void
        {
            Automation.registerDelegateClass(spark.components.NavigatorContent, SparkNavigatorContentAutomationImpl);
        }   
        
        //--------------------------------------------------------------------------
        //
        //  Constructor
        //
        //--------------------------------------------------------------------------
        
        /**
         *  Constructor.
         * @param obj NavigatorContent object to be automated.     
         *  
         *  @langversion 3.0
         *  @playerversion Flash 9
         *  @playerversion AIR 1.5
         *  @productversion Flex 4
         */
        public function SparkNavigatorContentAutomationImpl(obj:spark.components.NavigatorContent)
        {
            super(obj);
        }
        
        
    }
}