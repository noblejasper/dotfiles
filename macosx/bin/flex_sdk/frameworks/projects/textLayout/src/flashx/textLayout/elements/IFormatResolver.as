////////////////////////////////////////////////////////////////////////////////
//
// ADOBE SYSTEMS INCORPORATED
// Copyright 2007-2010 Adobe Systems Incorporated
// All Rights Reserved.
//
// NOTICE:  Adobe permits you to use, modify, and distribute this file 
// in accordance with the terms of the license agreement accompanying it.
//
////////////////////////////////////////////////////////////////////////////////
package flashx.textLayout.elements
{
	import flashx.textLayout.formats.ITextLayoutFormat;
	/** Interface to a format resolver. An implementation allows you to attach a styling mechanism of your choosing, such as
	 *  Flex CSS styling and named styles, to a TextFlow.
	 *
	 * @playerversion Flash 10
	 * @playerversion AIR 1.5
	 * @langversion 3.0
	 * 
	 * @see flashx.textLayout.elements.TextFlow#formatResolver TextFlow.formatResolver
	 */
	 
	public interface IFormatResolver 
	{
		/** Invalidates any cached formatting information for a TextFlow so that formatting must be recomputed.
		 *
		 * @playerversion Flash 10
		 * @playerversion AIR 1.5
	 	 * @langversion 3.0
	 	 */
	 	 
		function invalidateAll(textFlow:TextFlow):void;
		
		/** Invalidates cached formatting information on this element because, for example, the <code>parent</code> changed, 
		 *  or the <code>id</code> or the <code>styleName</code> changed or the <code>typeName</code> changed. 
		 *
		 * @playerversion Flash 10
	 	 * @playerversion AIR 1.5
	  	 * @langversion 3.0*/
	  	 
		function invalidate(target:Object):void;
		
		/** Given a FlowElement or ContainerController object, return any format settings for it.
		 *
		 * @return format settings for the specified object.
		 * 
		 * @playerversion Flash 10
		 * @playerversion AIR 1.5
		 * @langversion 3.0
	 	 */
	 	 
		function resolveFormat(target:Object):ITextLayoutFormat;
		
		/** Given a FlowElement or ContainerController object and the name of a format property, return the format value
		 * or <code>undefined</code> if the value is not found.
		 *
		 * @return the value of the specified format for the specified object.
		 *
		 * @playerversion Flash 10
		 * @playerversion AIR 1.5
		 * @langversion 3.0
		 */
		 
		function resolveUserFormat(target:Object,userFormat:String):*;
		
		/** Returns the format resolver when a TextFlow is copied.
		 *
		 * @return the format resolver for the copy of the TextFlow.
		 *
		 * @playerversion Flash 10
		 * @playerversion AIR 1.5
		 * @langversion 3.0
		 */
		 
		function getResolverForNewFlow(oldFlow:TextFlow,newFlow:TextFlow):IFormatResolver;
	}
}
