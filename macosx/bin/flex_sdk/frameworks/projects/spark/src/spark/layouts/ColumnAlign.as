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

package spark.layouts
{

/**
 *  The ColumnAlign class defines the possible values for the 
 *  <code>columnAlign</code> property of the TileLayout class.
 * 
 *  @see TileLayout#columnAlign
 *  
 *  @langversion 3.0
 *  @playerversion Flash 10
 *  @playerversion AIR 1.5
 *  @productversion Flex 4
 */
public final class ColumnAlign
{
    /**
     *  Do not justify the rows.
     *  
     *  @langversion 3.0
     *  @playerversion Flash 10
     *  @playerversion AIR 1.5
     *  @productversion Flex 4
     */
    public static const LEFT:String = "left";

    /**
     *  Justify the rows by increasing the vertical gap.
     *  
     *  @langversion 3.0
     *  @playerversion Flash 10
     *  @playerversion AIR 1.5
     *  @productversion Flex 4
     */
    public static const JUSTIFY_USING_GAP:String = "justifyUsingGap";

    /**
     *  Justify the rows by increasing the row height.
     *  
     *  @langversion 3.0
     *  @playerversion Flash 10
     *  @playerversion AIR 1.5
     *  @productversion Flex 4
     */
    public static const JUSTIFY_USING_WIDTH:String = "justifyUsingWidth";
}
}
