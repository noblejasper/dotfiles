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
	import flashx.textLayout.debug.assert;
	import flashx.textLayout.edit.ElementMark;
	import flashx.textLayout.edit.ElementRange;
	import flashx.textLayout.edit.IMemento;
	import flashx.textLayout.edit.MementoList;
	import flashx.textLayout.edit.ModelEdit;
	import flashx.textLayout.edit.ParaEdit;
	import flashx.textLayout.edit.SelectionState;
	import flashx.textLayout.elements.FlowElement;
	import flashx.textLayout.elements.FlowGroupElement;
	import flashx.textLayout.elements.FlowLeafElement;
	import flashx.textLayout.elements.ListElement;
	import flashx.textLayout.elements.ListItemElement;
	import flashx.textLayout.elements.ParagraphElement;
	import flashx.textLayout.elements.SubParagraphGroupElementBase;
	import flashx.textLayout.formats.IListMarkerFormat;
	import flashx.textLayout.formats.ITextLayoutFormat;
	import flashx.textLayout.formats.TextLayoutFormat;
	import flashx.textLayout.tlf_internal;

	use namespace tlf_internal;

	/**
	 * The CreateListOperation class encapsulates creating list
	 *
	 * @playerversion Flash 10
	 * @playerversion AIR 1.5
	 * @langversion 3.0 
	 */
	public class CreateListOperation extends FlowTextOperation
	{
		// describes the target
		private var _listParentMarker:ElementMark;
		private var _mementoList:MementoList;

		private var _listFormat:ITextLayoutFormat;
		private var _listElement:ListElement;		// the element that gets created
		
		private var _postOpSelectionState:SelectionState;
		
		/** 
		 * Creates an CreateListOperation object.
		 * 
		 * @playerversion Flash 10
		 * @playerversion AIR 1.5
	 	 * @langversion 3.0 
		 */
		public function CreateListOperation(operationState:SelectionState, parent:FlowGroupElement = null, listFormat:ITextLayoutFormat = null)
		{
			super(operationState);
			
			this.parent = parent;
			_listFormat = listFormat;
			_mementoList = new MementoList(operationState.textFlow);
		}
		
		/** 
		 * Specifies the element this operation adds a new ListElement to.
		 * @playerversion Flash 10
		 * @playerversion AIR 1.5
		 * @langversion 3.0 
		 */
		public function get parent():FlowGroupElement
		{
			return _listParentMarker ? _listParentMarker.findElement(originalSelectionState.textFlow) as FlowGroupElement : null;
		}
		public function set parent(value:FlowGroupElement):void
		{
			if (!value)
			{
				// descend to the lowest level non-paragraph element that contains both positions
				// effectively make the new list as close to the spans as possible
				value = textFlow;
				
				var begPos:int = this.absoluteStart;
				var endPos:int = this.absoluteEnd;
				for (;;)
				{
					var begChildIdx:int = value.findChildIndexAtPosition(begPos);
					var elem:FlowGroupElement = value.getChildAt(begChildIdx) as FlowGroupElement;
					if (elem is ParagraphElement)
						break;
					begPos -= elem.parentRelativeStart;
					endPos -= elem.parentRelativeStart;
					if (endPos >= elem.textLength)	// end pos is in the next element
						break;
					value = elem;
				}
			}
			else if (value is SubParagraphGroupElementBase)
				value = value.getParagraph().parent;
			
			_listParentMarker = new ElementMark(value,0);
		}

		/** TextLayoutFormat to be applied to the new ListElement. 
		 * @playerversion Flash 10
		 * @playerversion AIR 1.5
		 * @langversion 3.0 
		 */	
		public function get listFormat():ITextLayoutFormat
		{ return _listFormat; }
		public function set listFormat(value:ITextLayoutFormat):void
		{ _listFormat = value; }
		
		/** The new ListElement. 
		 * @playerversion Flash 10
		 * @playerversion AIR 1.5
		 * @langversion 3.0 
		 */	
		public function get newListElement():ListElement
		{ return _listElement; }
		
		/** @private */
		public override function doOperation():Boolean
		{
			var target:FlowGroupElement = parent;
			
			// find the starting child that's going to be in the list and 
			var begChildIndex:int = 0;
			var begStart:int = absoluteStart - target.getAbsoluteStart();
			CONFIG::debug { assert(begStart >= 0 && begStart < target.textLength,"CreateListOperation: bad target"); }
			
			var endChildIndex:int;
			var endStart:int = absoluteEnd - target.getAbsoluteStart();
			CONFIG::debug { assert(endStart >= 0 && endStart <= target.textLength,"CreateListOperation: bad target"); }
			
			// scratch vars
			var child:FlowGroupElement;
			
			if (begStart > 0)
			{
				// figure out the starting child
				begChildIndex = target.findChildIndexAtPosition(begStart);
				child = target.getChildAt(begChildIndex) as FlowGroupElement;
				if (child.parentRelativeStart != begStart)
				{					
					_mementoList.push(ModelEdit.splitElement(textFlow,child,begStart-child.parentRelativeStart));
					
					if (child is ParagraphElement)
						endStart++;
					begChildIndex++;
				}
			}
			
			if (endStart >= 0)
			{
				if (endStart >= target.textLength - 1)
					endChildIndex = target.numChildren;
				else
				{
					// figure out the starting child
					endChildIndex = target.findChildIndexAtPosition(endStart);
					child = target.getChildAt(endChildIndex) as FlowGroupElement;
					if (child.parentRelativeStart != endStart)			
					{
						_mementoList.push(ModelEdit.splitElement(textFlow,child,endStart-child.parentRelativeStart));
						endChildIndex++;
					}
				}
			}
			else
				endChildIndex = begChildIndex+1;
			
			_listElement = new ListElement;
			_listElement.format = listFormat;
			
			var listItem:ListItemElement;
			
			if (begChildIndex == target.numChildren)
			{
				// new list at the end of target
				child = target.getChildAt(target.numChildren-1) as FlowGroupElement;
				
				_mementoList.push(ModelEdit.splitElement(textFlow,child,child.textLength));
				_mementoList.push(ModelEdit.addElement(textFlow,_listElement,target,target.numChildren));

				if (!(child is ListItemElement))
				{
					listItem = new ListItemElement();	// NO PMD
					_mementoList.push(ModelEdit.addElement(textFlow,listItem,_listElement,_listElement.numChildren));
					_mementoList.push(ModelEdit.moveElement(textFlow,child,listItem,listItem.numChildren));
					// normalize does this
					if (listItem.normalizeNeedsInitialParagraph())
						_mementoList.push(ModelEdit.addElement(textFlow,new ParagraphElement,listItem,0));
				}
				else
					_mementoList.push(ModelEdit.moveElement(textFlow,child,_listElement,_listElement.numChildren));
			}
			else
			{
				_mementoList.push(ModelEdit.addElement(textFlow,_listElement,target,endChildIndex));
				
				// normalize does this
				if ((target is ListItemElement) && (target as ListItemElement).normalizeNeedsInitialParagraph())
				{
					_mementoList.push(ModelEdit.addElement(textFlow,new ParagraphElement,target,0));
					begChildIndex++;
					endChildIndex++;
				}

				if (begChildIndex == endChildIndex)
				{
					listItem = new ListItemElement();	// No PMD
					_mementoList.push(ModelEdit.addElement(textFlow,listItem,_listElement,0));
					_mementoList.push(ModelEdit.addElement(textFlow,new ParagraphElement,listItem,0));					
				}
				else
				{
					while (begChildIndex < endChildIndex)
					{
						child = target.getChildAt(begChildIndex) as FlowGroupElement;
						if (child is ListItemElement)
						{
							listItem = child as ListItemElement;
							_mementoList.push(ModelEdit.moveElement(textFlow,listItem,_listElement,_listElement.numChildren));
							if (!(listItem.getChildAt(0) is ParagraphElement))
								_mementoList.push(ModelEdit.addElement(textFlow,new ParagraphElement,listItem,0));
						}
						else
						{
							listItem = new ListItemElement();	// No PMD
							_mementoList.push(ModelEdit.addElement(textFlow,listItem,_listElement,_listElement.numChildren));
							_mementoList.push(ModelEdit.moveElement(textFlow,child,listItem,listItem.numChildren));
							// normalize does this
							if (listItem.normalizeNeedsInitialParagraph())
								_mementoList.push(ModelEdit.addElement(textFlow,new ParagraphElement,listItem,0));
	
							child = listItem;
						}
						child.normalizeRange(0,child.textLength);
						endChildIndex--;
					}
				}
				// normalize does this
				var testListItem:ListItemElement = target as ListItemElement;
				if (testListItem && testListItem.normalizeNeedsInitialParagraph())
					_mementoList.push(ModelEdit.addElement(textFlow,new ParagraphElement,testListItem,0));
				testListItem = target.parent as ListItemElement;
				if (testListItem && testListItem.normalizeNeedsInitialParagraph())
					_mementoList.push(ModelEdit.addElement(textFlow,new ParagraphElement,testListItem,0));
			}
			
			if (originalSelectionState.selectionManagerOperationState && textFlow.interactionManager)
			{
				textFlow.normalize();
				_postOpSelectionState = new SelectionState(textFlow,_listElement.getAbsoluteStart(),_listElement.getAbsoluteStart()+_listElement.textLength-1);
				textFlow.interactionManager.setSelectionState(_postOpSelectionState);
			}
			
			return true;
		}

		/** @private */
		public override function undo():SelectionState
		{
			_mementoList.undo();
			return originalSelectionState; 
		}
		
		/** @private */
		public override function redo():SelectionState
		{
			_mementoList.redo();
			return _postOpSelectionState; 
			
		}
	}
}
