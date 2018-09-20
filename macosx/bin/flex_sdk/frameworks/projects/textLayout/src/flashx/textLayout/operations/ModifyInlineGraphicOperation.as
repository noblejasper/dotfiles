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
package flashx.textLayout.operations
{
	import flashx.textLayout.edit.SelectionState;
	import flashx.textLayout.elements.FlowElement;
	import flashx.textLayout.elements.FlowGroupElement;
	import flashx.textLayout.elements.InlineGraphicElement;
	import flashx.textLayout.tlf_internal;

	use namespace tlf_internal;


	/**
	 * The InsertInlineGraphicOperation class encapsulates the modification of an existing inline graphic.
	 *
	 * @see flashx.textLayout.elements.InlineGraphicElement
	 * @see flashx.textLayout.edit.EditManager
	 * @see flashx.textLayout.events.FlowOperationEvent
	 * 
	 * @playerversion Flash 10
	 * @playerversion AIR 1.5
	 * @langversion 3.0 
	 */
	public class ModifyInlineGraphicOperation extends FlowTextOperation
	{ 
		private var _source:Object;
		private var imageWidth:Object;
		private var imageHeight:Object;
		private var _options:Object;
		private var oldImage:FlowElement;
		private var selPos:int = 0;
		
		/** 
		 * Creates a ModifyInlineGraphicsOperation object.
		 * 
		 * @param operationState Describes the insertion point. 
		 * If a range is selected, the operation deletes the contents of that range.
		 * @param	source	The graphic source (uri string, URLRequest, DisplayObject, or Class of an embedded asset). 
		 * @param	width	The width to assign (number of pixels, percent, or the string 'auto')
		 * @param	height	The height to assign (number of pixels, percent, or the string 'auto')
		 * @param	options	None supported
		 * 
		 * @playerversion Flash 10
		 * @playerversion AIR 1.5
	 	 * @langversion 3.0 
		 */
		function ModifyInlineGraphicOperation(operationState:SelectionState, source:Object, width:Object, height:Object, options:Object = null)
		{
			super(operationState);
							
			_source = source;
			_options = options;
			imageWidth = width;
			imageHeight = height;
		}
		
		/**	
		 * @copy flashx.textLayout.elements.InlineGraphicElement#source
		 * 
		 * @playerversion Flash 10
		 * @playerversion AIR 1.5
	 	 * @langversion 3.0 
 		 */
		public function get source():Object
		{
			return _source;
		}
		public function set source(value:Object):void
		{
			_source = value;
		}

		/** 
		 * @copy flashx.textLayout.elements.InlineGraphicElement#width
		 * 
		 * @playerversion Flash 10
		 * @playerversion AIR 1.5
	 	 * @langversion 3.0 
		*/
		public function get width():Object
		{
			return imageWidth;
		}
		public function set width(value:Object):void
		{
			imageWidth = value;
		}

		/** 
		 * @copy flashx.textLayout.elements.InlineGraphicElement#height
		 * 
		 * @playerversion Flash 10
		 * @playerversion AIR 1.5
	 	 * @langversion 3.0 
		*/
		public function get height():Object
		{
			return imageHeight;
		}
		public function set height(value:Object):void
		{
			imageHeight = value;
		}
		
		/** 
		 * options are not supported
		 * 
		 * @playerversion Flash 10
		 * @playerversion AIR 1.5
	 	 * @langversion 3.0 
		*/
		public function get options():Object
		{
			return _options;
		}
		public function set options(value:Object):void
		{
			_options = value;
		}
		
		/** @private */
		public override function doOperation():Boolean
		{
			selPos = absoluteStart;
			var img:InlineGraphicElement = textFlow.findLeaf(selPos) as InlineGraphicElement;
			if (img)
			{
				oldImage = img.shallowCopy(0,1);
				// only update changed things
				if (img.width != imageWidth)
					img.width = imageWidth;
				if (img.height != imageHeight)
					img.height = imageHeight;
				if (img.source != _source)
					img.source = _source;
				if (options && img.float != options.toString())
					img.float = options.toString();
			}
			
			return true;	
		}
	
		/** @private */
		public override function undo():SelectionState
		{
			var leafNode:FlowElement = textFlow.findLeaf(selPos);
			var leafNodeParent:FlowGroupElement = leafNode.parent;
			var elementIdx:int = leafNode.parent.getChildIndex(leafNode);
			leafNodeParent.replaceChildren(elementIdx, elementIdx + 1, oldImage);			

			return originalSelectionState; 
		}
	}
}