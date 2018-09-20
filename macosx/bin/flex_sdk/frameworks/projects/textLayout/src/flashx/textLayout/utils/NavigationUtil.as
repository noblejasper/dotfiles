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
package flashx.textLayout.utils
{
	import flash.geom.Point;
	import flash.geom.Rectangle;
	import flash.text.engine.TextLine;
	import flash.text.engine.TextRotation;
	
	import flashx.textLayout.compose.IFlowComposer;
	import flashx.textLayout.compose.TextFlowLine;
	import flashx.textLayout.container.ContainerController;
	import flashx.textLayout.container.ScrollPolicy;
	import flashx.textLayout.elements.FlowLeafElement;
	import flashx.textLayout.elements.ParagraphElement;
	import flashx.textLayout.elements.TextFlow;
	import flashx.textLayout.elements.TextRange;
	import flashx.textLayout.formats.BlockProgression;
	import flashx.textLayout.formats.Direction;
	import flashx.textLayout.tlf_internal;
	
	use namespace tlf_internal;
	

	/** 
	 * Utilities for manipulating a TextRange 
	 * The methods of this class are static and must be called using
	 * the syntax <code>NavigationUtil.method(<em>parameter</em>)</code>.
	 *
	 * @playerversion Flash 10
	 * @playerversion AIR 1.5
	 * @langversion 3.0
	 */
	public final class NavigationUtil 
	{
		private static function validateTextRange(range:TextRange):Boolean
		{ return range.textFlow != null && range.anchorPosition != -1 && range.activePosition != -1; }
		
		private static function doIncrement(flowRoot:TextFlow, pos:int, incrementer:Function):int
		{
			var para:ParagraphElement = flowRoot.findAbsoluteParagraph(pos);
			return incrementer(flowRoot, para, pos, para.getAbsoluteStart());			
		}
		
		private static function previousAtomHelper(flowRoot:TextFlow, para:ParagraphElement, pos:int, paraStart:int):int
		{
			if (pos - paraStart == 0)
				return (pos > 0) ? pos - 1 : 0;
			return para.findPreviousAtomBoundary(pos - paraStart) + paraStart;
		}
		
		/** 
		 * Returns the absolute position of the previous atom. 
		 * @playerversion Flash 10
		 * @playerversion AIR 1.5
		 * @langversion 3.0
		 */
		public static function previousAtomPosition(flowRoot:TextFlow, absolutePos:int):int
		{
			return doIncrement(flowRoot,absolutePos,previousAtomHelper);
		}
	
		private static function nextAtomHelper(flowRoot:TextFlow, para:ParagraphElement, pos:int, paraStart:int):int
		{
			if (pos - paraStart == para.textLength - 1)
				return Math.min(flowRoot.textLength, pos + 1);
			return para.findNextAtomBoundary(pos - paraStart) + paraStart;
		}
		
		/** 
		 * Returns the absolute position of the next atom.
		 * @playerversion Flash 10
		 * @playerversion AIR 1.5
		 * @langversion 3.0
		 */
		public static function nextAtomPosition(flowRoot:TextFlow, absolutePos:int):int
		{
			return doIncrement(flowRoot,absolutePos,nextAtomHelper);
		}
	
		/** 
		 * Returns absolute position of the beginning of the previous word.
		 * @playerversion Flash 10
		 * @playerversion AIR 1.5
		 * @langversion 3.0
		 */
		public static function previousWordPosition(flowRoot:TextFlow, absolutePos:int):int
		{
			if (isOverset(flowRoot, absolutePos))
				return endOfLastController(flowRoot);
			
			var para:ParagraphElement = flowRoot.findAbsoluteParagraph(absolutePos);
			var paraStart:int = para.getAbsoluteStart();
			
			var prevWordPos:int = absolutePos - paraStart;
			var nextWordPos:int = 0;
			if (absolutePos - paraStart == 0)
				return (absolutePos > 0) ? absolutePos - 1 : 0;
			do
			{
				nextWordPos = para.findPreviousWordBoundary(prevWordPos);
				if (prevWordPos == nextWordPos) prevWordPos = para.findPreviousWordBoundary(prevWordPos - 1);
				else prevWordPos = nextWordPos;
			} while (prevWordPos > 0 && CharacterUtil.isWhitespace(para.getCharCodeAtPosition(prevWordPos)));
			return prevWordPos + paraStart;
		}
	
		/** 
		 * Returns the absolute position of the beginning of the next word.
		 * @playerversion Flash 10
		 * @playerversion AIR 1.5
		 * @langversion 3.0
		 */
		public static function nextWordPosition(flowRoot:TextFlow, absolutePos:int):int
		{
			var para:ParagraphElement = flowRoot.findAbsoluteParagraph(absolutePos);
			var paraStart:int = para.getAbsoluteStart();
			
			var nextWordPos:int = absolutePos - paraStart;
			
			if (absolutePos - paraStart == para.textLength - 1)
				return Math.min(flowRoot.textLength, absolutePos + 1);
			do
			{
				nextWordPos = para.findNextWordBoundary(nextWordPos);
			} while (nextWordPos < (para.textLength - 1) && CharacterUtil.isWhitespace(para.getCharCodeAtPosition(nextWordPos)))
			return nextWordPos + paraStart;
		}
		
		/** @private */
		static tlf_internal function updateStartIfInReadOnlyElement(textFlow:TextFlow, idx:int):int
		{
			return idx;
		}
		
		/** @private */
		static tlf_internal function updateEndIfInReadOnlyElement(textFlow:TextFlow, idx:int):int
		{
			return idx; 
		}
		
		static private function moveForwardHelper(range:TextRange, extendSelection:Boolean, incrementor:Function):Boolean
		{
			var textFlow:TextFlow = range.textFlow;
			var begIdx:int = range.anchorPosition;
			var endIdx:int = range.activePosition;
				
			if (extendSelection) 
				endIdx = incrementor(textFlow, endIdx);
			else {
				if (begIdx == endIdx) //no selection, just move insertion point
				{
					begIdx = incrementor(textFlow, begIdx);
					endIdx = begIdx;
				}
				else if (endIdx > begIdx) //change selection to insertion point
					begIdx = endIdx;
				else
					endIdx = begIdx;
			}
			
			if (begIdx == endIdx)
			{
				begIdx = updateStartIfInReadOnlyElement(textFlow, begIdx);
				endIdx = updateEndIfInReadOnlyElement(textFlow, endIdx);
			} else {
				endIdx = updateEndIfInReadOnlyElement(textFlow, endIdx);
			} 
					
			if (!extendSelection && (range.anchorPosition == begIdx) && (range.activePosition == endIdx))
			{
				if (begIdx < endIdx)
				{
					begIdx = Math.min(endIdx + 1, textFlow.textLength - 1);
					endIdx = begIdx;
				}else {
					endIdx = Math.min(begIdx + 1, textFlow.textLength - 1);
					begIdx = endIdx;
				}	
			}
			return range.updateRange(begIdx,endIdx);							 	
		}
		
		static private function moveBackwardHelper(range:TextRange, extendSelection:Boolean, incrementor:Function):Boolean
		{
			var textFlow:TextFlow = range.textFlow;
			var begIdx:int = range.anchorPosition;
			var endIdx:int = range.activePosition;
				
			if (extendSelection)	//shift key is pressed 
				endIdx = incrementor(textFlow, endIdx);
			else {
				if (begIdx == endIdx) //no selection, just move insertion point
				{
					begIdx = incrementor(textFlow, begIdx);
					endIdx = begIdx;
				}
				else if (endIdx > begIdx) //change selection to insertion point
					endIdx = begIdx;
				else
					begIdx = endIdx;
			}
					
			if (begIdx == endIdx)
			{
				begIdx = updateEndIfInReadOnlyElement(textFlow, begIdx);
				endIdx = updateStartIfInReadOnlyElement(textFlow, endIdx);
			} else {
				endIdx = updateStartIfInReadOnlyElement(textFlow, endIdx);
			} 
					
			if (!extendSelection && (range.anchorPosition == begIdx) && (range.activePosition == endIdx))
			{
				if (begIdx < endIdx)
				{
					endIdx = Math.max(begIdx - 1, 0);
					begIdx = endIdx;
				}else {
					begIdx = Math.max(endIdx - 1, 0);
					endIdx = begIdx;
				}	
			}
			return range.updateRange(begIdx,endIdx);
		 }
		
		/**
		 * Sets the TextRange forward by one character.
		 * @param extendSelection	Indicates that only activeIndex should move
		 * @return true if selection changed.
		 * @playerversion Flash 10
		 * @playerversion AIR 1.5
		 * @langversion 3.0
		 */		 
		static public function nextCharacter(range:TextRange, extendSelection:Boolean = false):Boolean
		{
			if (validateTextRange(range))
			{
				if (!adjustForOversetForward(range))
					moveForwardHelper(range, extendSelection, nextAtomPosition);
				return true;
			}
		 	return false;
		}
		 
		/**
		 * Sets the TextRange backward by one character.
		 * @param extendSelection	Indicates that only activeIndex should move
		 * @return true if selection changed.
		 * @playerversion Flash 10
		 * @playerversion AIR 1.5
		 * @langversion 3.0
		 */		 		 
		static public function previousCharacter(range:TextRange, extendSelection:Boolean = false):Boolean
		{
			if (validateTextRange(range))
			{
				if (!adjustForOversetBack(range))
					moveBackwardHelper(range, extendSelection, previousAtomPosition);
				return true;
			} 
			return false;
		} 
		/**
		 * Sets the TextRange forward by one word.
		 * @param extendSelection	Indicates that only activeIndex should move
		 * @return true if selection changed.
		 * @playerversion Flash 10
		 * @playerversion AIR 1.5
		 * @langversion 3.0
		 */		 		 		 
		static public function nextWord(range:TextRange, extendSelection:Boolean = false):Boolean
		{
			if (validateTextRange(range))
			{
				if (!adjustForOversetForward(range))
					moveForwardHelper(range, extendSelection, nextWordPosition);
				return true;
			}
			return false;
		}
		 
		/**
		 * Sets the TextRange backward by one word.
		 * @param extendSelection	Indicates that only activeIndex should move
		 * @return true if selection changed.
		 * @playerversion Flash 10
		 * @playerversion AIR 1.5
		 * @langversion 3.0
		 */		 		 		 		 
		static public function previousWord(range:TextRange, extendSelection:Boolean = false):Boolean
		{
			if (validateTextRange(range))
			{
				if (!adjustForOversetBack(range))
		 			moveBackwardHelper(range, extendSelection, previousWordPosition);
		 		return true;
		 	}
		 	return false;
		} 
		
		/** @private */
		static tlf_internal function computeEndIdx(targetFlowLine:TextFlowLine,curTextFlowLine:TextFlowLine,blockProgression:String,isRTLDirection:Boolean,globalPoint:Point):int
		{
			var endIdx:int;
			var targetTextLine:TextLine = targetFlowLine.getTextLine(true);
			var currentTextLine:TextLine = curTextFlowLine.getTextLine(true);
			var bidiRightToLeft:Boolean = ((currentTextLine.getAtomBidiLevel(atomIndex) % 2) != 0); 				
			
			if (targetFlowLine.controller == curTextFlowLine.controller)
			{
				if(blockProgression != BlockProgression.RL)
				{
					globalPoint.y -= (currentTextLine.y - targetTextLine.y);
				} else {
					globalPoint.x += (targetTextLine.x - currentTextLine.x);
				}
			} else {
				var firstAtomRect:Rectangle = targetTextLine.getAtomBounds(0);
				var firstAtomPoint:Point = new Point();
				firstAtomPoint.x = firstAtomRect.left;
				firstAtomPoint.y = 0;
				firstAtomPoint = targetTextLine.localToGlobal(firstAtomPoint);
				if(blockProgression != BlockProgression.RL)
				{
					globalPoint.x -= curTextFlowLine.controller.container.x;
					globalPoint.y = firstAtomPoint.y;
				} else {
					globalPoint.x = firstAtomPoint.x;
					globalPoint.y -= curTextFlowLine.controller.container.y; 
				}					
			} 
			
			var atomIndex:int = targetTextLine.getAtomIndexAtPoint(globalPoint.x,globalPoint.y);
			if (atomIndex == -1)
			{
				if(blockProgression != BlockProgression.RL) {
					if (!bidiRightToLeft)
						endIdx = (globalPoint.x <= targetTextLine.x) ? targetFlowLine.absoluteStart : (targetFlowLine.absoluteStart + targetFlowLine.textLength - 1);
					else
						endIdx = (globalPoint.x <= targetTextLine.x) ? (targetFlowLine.absoluteStart + targetFlowLine.textLength - 1) : targetFlowLine.absoluteStart;						
				} else {
					if (!bidiRightToLeft)
						endIdx = (globalPoint.y <= targetTextLine.y) ? targetFlowLine.absoluteStart : (targetFlowLine.absoluteStart + targetFlowLine.textLength - 1);
					else
						endIdx = (globalPoint.y <= targetTextLine.y)  ? (targetFlowLine.absoluteStart + targetFlowLine.textLength - 1) : targetFlowLine.absoluteStart;						
				}
			} 
			else {
				// get the character box and if check we are past the middle select past this character. 
				var glyphRect:Rectangle = targetTextLine.getAtomBounds(atomIndex);
				var leanRight:Boolean = false;
				if(glyphRect)
				{	
					//if this is TTB and NOT TCY determine lean based on Y coordinates...
					var glyphGlobalPoint:Point = new Point();
					glyphGlobalPoint.x = glyphRect.x;
					glyphGlobalPoint.y = glyphRect.y;
					glyphGlobalPoint = targetTextLine.localToGlobal(glyphGlobalPoint);
					
					if((blockProgression == BlockProgression.RL) && targetTextLine.getAtomTextRotation(atomIndex) != TextRotation.ROTATE_0)
						leanRight = (globalPoint.y > (glyphGlobalPoint.y + glyphRect.height/2));
					else //use X..
						leanRight = (globalPoint.x > (glyphGlobalPoint.x + glyphRect.width/2));
				}
				
				var paraSelectionIdx:int;
				if ((targetTextLine.getAtomBidiLevel(atomIndex) % 2) != 0) // Right to left case, right is "start" unicode
					paraSelectionIdx = leanRight ? targetTextLine.getAtomTextBlockBeginIndex(atomIndex) : targetTextLine.getAtomTextBlockEndIndex(atomIndex);
				else  {// Left to right case, right is "end" unicode
					if (isRTLDirection) {
						if ((leanRight == false) && (atomIndex > 0))
						{
							paraSelectionIdx = targetTextLine.getAtomTextBlockBeginIndex(atomIndex - 1);	
						} else {
							paraSelectionIdx = targetTextLine.getAtomTextBlockBeginIndex(atomIndex);
						}
					} else {
						paraSelectionIdx = leanRight ? targetTextLine.getAtomTextBlockEndIndex(atomIndex) : targetTextLine.getAtomTextBlockBeginIndex(atomIndex);							
					}
				}
				endIdx = targetFlowLine.paragraph.getAbsoluteStart() + paraSelectionIdx;
			}
			return endIdx;
		}
		/**
		 * Sets the TextRange down one line
		 * @param extendSelection	Indicates that only activeIndex should move
		 * @return true if selection changed.
		 * @playerversion Flash 10
		 * @playerversion AIR 1.5
		 * @langversion 3.0
		 */		 		 		 		 		 
		static public function nextLine(range:TextRange, extendSelection:Boolean = false):Boolean
		{
			if (!validateTextRange(range))
				return false;
		
			if (adjustForOversetForward(range))
				return true;
				
			var textFlow:TextFlow = range.textFlow;
			var blockProgression:String = textFlow.computedFormat.blockProgression;
			var begIdx:int = range.anchorPosition;
			var endIdx:int = range.activePosition;
			var limitIdx:int = endOfLastController(textFlow);
			
			var curLine:int = textFlow.flowComposer.findLineIndexAtPosition(endIdx);
			var isRTLDirection:Boolean = (textFlow.computedFormat.direction == Direction.RTL);			

			if (curLine < textFlow.flowComposer.numLines-1)	//create, expand or shrink the selection
			{
				var curTextFlowLine:TextFlowLine = textFlow.flowComposer.getLineAt(curLine);
				var lineStart:int = curTextFlowLine.absoluteStart;
				var lineDelta:int = endIdx - lineStart;
				var currentTextLine:TextLine = curTextFlowLine.getTextLine(true);
				var para:ParagraphElement = curTextFlowLine.paragraph;
				var atomIndex:int = currentTextLine.getAtomIndexAtCharIndex(endIdx - para.getAbsoluteStart());
				var bidiRightToLeft:Boolean = ((currentTextLine.getAtomBidiLevel(atomIndex) % 2) != 0); 
				var curPosRect:Rectangle = currentTextLine.getAtomBounds(atomIndex);
				var currentTextLineX:Number = currentTextLine.x;
				var curPosRectLeft:Number = curPosRect.left;
				var curPosRectRight:Number = curPosRect.right;
				if(blockProgression == BlockProgression.RL)
				{
					currentTextLineX = currentTextLine.y;
					curPosRectLeft = curPosRect.top;
					curPosRectRight = curPosRect.bottom;
				}
				
				//find the atom
				var globalPoint:Point = new Point();				
						
				if(blockProgression != BlockProgression.RL)
				{
					if (!isRTLDirection)
						globalPoint.x = curPosRect.left;
					else
						globalPoint.x = curPosRect.right;
					globalPoint.y = 0;
				} else {						
					globalPoint.x = 0;
					if (!isRTLDirection)
						globalPoint.y = curPosRect.top;
					else
						globalPoint.y = curPosRect.bottom;
				}
				
				globalPoint = currentTextLine.localToGlobal(globalPoint);
				
				//at this point, we have the global point of our current position.  Now adjust x or y to the
				//baseline of the next line.
				var nextFlowLine:TextFlowLine = textFlow.flowComposer.getLineAt(curLine + 1);
				if (nextFlowLine.absoluteStart >= limitIdx)
				{
					if (!extendSelection)
						range.activePosition = range.anchorPosition = textFlow.textLength - 1;
					else
						range.activePosition = textFlow.textLength;
					return true;
				}
				
				
				// get the last container so that we can make sure the previous line is in view.
				var controller:ContainerController = textFlow.flowComposer.getControllerAt(textFlow.flowComposer.numControllers-1);
				var firstPosInContainer:int = controller.absoluteStart;
				var lastPosInContainer:int = firstPosInContainer + controller.textLength;
				if ((nextFlowLine.absoluteStart >= firstPosInContainer) && (nextFlowLine.absoluteStart < lastPosInContainer))
				{
					if (nextFlowLine.isDamaged())
					{
						textFlow.flowComposer.composeToPosition(nextFlowLine.absoluteStart+1);
						nextFlowLine = textFlow.flowComposer.getLineAt(curLine + 1);
						if (nextFlowLine.isDamaged())
							return false;
					}
					// Scroll down one line, but allow scrolling only in the block progression direction
					var curLogicalHorizontalScrollPos:Number = (blockProgression == BlockProgression.TB) ? controller.horizontalScrollPosition : controller.verticalScrollPosition;
					controller.scrollToRange(nextFlowLine.absoluteStart, nextFlowLine.absoluteStart + nextFlowLine.textLength - 1);
					if (blockProgression == BlockProgression.TB)
						controller.horizontalScrollPosition = curLogicalHorizontalScrollPos;
					else
						controller.verticalScrollPosition = curLogicalHorizontalScrollPos; 
				}
				
				endIdx = computeEndIdx(nextFlowLine,curTextFlowLine,blockProgression,isRTLDirection,globalPoint);

				if (endIdx >= textFlow.textLength)
					endIdx = textFlow.textLength;
			}
			else
				endIdx = textFlow.textLength;
				
			if (!extendSelection)
				begIdx = endIdx;
											
			if (begIdx == endIdx)
			{
				begIdx = updateStartIfInReadOnlyElement(textFlow, begIdx);
				endIdx = updateEndIfInReadOnlyElement(textFlow, endIdx);
			} else {
				endIdx = updateEndIfInReadOnlyElement(textFlow, endIdx);
			}
			return range.updateRange(begIdx,endIdx);			
		}
		  
		/**
		 * Sets the TextRange up one line.
		 * @param extendSelection	Indicates that only activeIndex should move
		 * @return true if selection changed.
		 * @playerversion Flash 10
		 * @playerversion AIR 1.5
		 * @langversion 3.0
		 */		 		 		 		 		 		 
		static public function previousLine(range:TextRange, extendSelection:Boolean = false):Boolean
		{
			if (!validateTextRange(range))
				return false;
			
			if (adjustForOversetBack(range))
				return true;
				
			var textFlow:TextFlow = range.textFlow;
			var blockProgression:String = textFlow.computedFormat.blockProgression;
			var begIdx:int = range.anchorPosition;
			var endIdx:int = range.activePosition;
			
			var curLine:int = textFlow.flowComposer.findLineIndexAtPosition(endIdx);
			var isRTLDirection:Boolean = (textFlow.computedFormat.direction == Direction.RTL);			

			if (curLine > 0)	//create, expand or shrink the selection
			{
				var curTextFlowLine:TextFlowLine = textFlow.flowComposer.getLineAt(curLine);
				var lineStart:int = curTextFlowLine.absoluteStart;
				var lineDelta:int = endIdx - lineStart;
				var currentTextLine:TextLine = curTextFlowLine.getTextLine(true);
				var para:ParagraphElement = curTextFlowLine.paragraph;
				var atomIndex:int = currentTextLine.getAtomIndexAtCharIndex(endIdx - para.getAbsoluteStart());
				var curPosRect:Rectangle = currentTextLine.getAtomBounds(atomIndex);
				var currentTextLineX:Number = currentTextLine.x;
				var curPosRectLeft:Number = curPosRect.left;
				var curPosRectRight:Number = curPosRect.right;
				if(blockProgression == BlockProgression.RL)
				{
					currentTextLineX = currentTextLine.y;
					curPosRectLeft = curPosRect.top;
					curPosRectRight = curPosRect.bottom;
				}
				
				//find the atom
				var globalPoint:Point = new Point();				
						
				if(blockProgression != BlockProgression.RL)
				{
					if (!isRTLDirection)
						globalPoint.x = curPosRect.left;
					else
						globalPoint.x = curPosRect.right;
					globalPoint.y = 0;
				} else {						
					globalPoint.x = 0;
					if (!isRTLDirection)
						globalPoint.y = curPosRect.top;
					else
						globalPoint.y = curPosRect.bottom;
				}
				
				globalPoint = currentTextLine.localToGlobal(globalPoint);
				
				//at this point, we have the global point of our current position.  Now adjust x or y to the
				//baseline of the next line.
				var prevFlowLine:TextFlowLine = textFlow.flowComposer.getLineAt(curLine - 1);
				// get the last container so that we can make sure the previous line is in view.
				var controller:ContainerController = textFlow.flowComposer.getControllerAt(textFlow.flowComposer.numControllers-1);
				var firstPosInContainer:int = controller.absoluteStart;
				var lastPosInContainer:int = firstPosInContainer + controller.textLength;
				if ((prevFlowLine.absoluteStart >= firstPosInContainer) && (prevFlowLine.absoluteStart < lastPosInContainer))
				{
					// Scroll up one line, but allow scrolling only in the block progression direction
					var curLogicalHorizontalScrollPos:Number = (blockProgression == BlockProgression.TB) ? controller.horizontalScrollPosition : controller.verticalScrollPosition;
					controller.scrollToRange(prevFlowLine.absoluteStart,prevFlowLine.absoluteStart+prevFlowLine.textLength-1);
					if (blockProgression == BlockProgression.TB)
						controller.horizontalScrollPosition = curLogicalHorizontalScrollPos;
					else
						controller.verticalScrollPosition = curLogicalHorizontalScrollPos; 
				}
				
				endIdx = computeEndIdx(prevFlowLine,curTextFlowLine,blockProgression,isRTLDirection,globalPoint);
			}
			else 
			{
				endIdx = 0;
			}
				
			if (!extendSelection)
				begIdx = endIdx;
											
			if (begIdx == endIdx)
			{
				begIdx = updateStartIfInReadOnlyElement(textFlow, begIdx);
				endIdx = updateEndIfInReadOnlyElement(textFlow, endIdx);
			} else {
				endIdx = updateEndIfInReadOnlyElement(textFlow, endIdx);
			}
			return range.updateRange(begIdx,endIdx);			
		}
		 
		/**
		 * Sets the TextRange down one page.
		 * @param extendSelection	Indicates that only activeIndex should move
		 * @return true if selection changed.
		 * @playerversion Flash 10
		 * @playerversion AIR 1.5
		 * @langversion 3.0
		 */		 		 		 		 		 		 		 
		static public function nextPage(range:TextRange, extendSelection:Boolean = false):Boolean
		{
			var controller:ContainerController;
			
			if (!validateTextRange(range))
				return false;
				
			var textFlow:TextFlow = range.textFlow;
			
			// if not the last container go to the beginning of the next container
			var controllerIndex:int = textFlow.flowComposer.findControllerIndexAtPosition(range.activePosition);
			if (controllerIndex != textFlow.flowComposer.numControllers-1)
			{
				range.activePosition = textFlow.flowComposer.getControllerAt(controllerIndex+1).absoluteStart;
				if (!extendSelection)
					range.anchorPosition = range.activePosition;
				return true;
			}
			
			if (!isScrollable(textFlow, range.activePosition))		// paging applies only to containers that scroll
				return false;
								
			if (adjustForOversetForward(range))
				return true;
		 	
			var begIdx:int = range.absoluteStart;
			var endIdx:int = range.absoluteEnd;
			var curLine:int = textFlow.flowComposer.findLineIndexAtPosition(endIdx);
			var curTextFlowLine:TextFlowLine = textFlow.flowComposer.getLineAt(curLine);
			
			var lineStart:int = textFlow.flowComposer.getLineAt(curLine).absoluteStart;
			var linePos:int = endIdx - lineStart;
 			var nextLine:int;
 			var nextTextFlowLine:TextFlowLine = curTextFlowLine;
 											
			var isTTB:Boolean = textFlow.computedFormat.blockProgression == BlockProgression.RL;
			var amount:Number;
			
			// get the last container
			controller = textFlow.flowComposer.getControllerAt(textFlow.flowComposer.numControllers-1);

			if (isTTB)
			{
				amount = controller.compositionWidth * textFlow.configuration.scrollPagePercentage;
			} else {
				amount = controller.compositionHeight * textFlow.configuration.scrollPagePercentage;
			}
  								
 			if (isTTB)
 			{
				var contentWidth:Number = controller.contentWidth;
 				if ((controller.horizontalScrollPosition - amount) < -contentWidth)
 				{
 					controller.horizontalScrollPosition = -contentWidth;
					nextLine = textFlow.flowComposer.numLines - 1;
					nextTextFlowLine = textFlow.flowComposer.getLineAt(nextLine);								
 				} else
 				{
 					var oldHorzScrollPos:Number = controller.horizontalScrollPosition;
 					controller.horizontalScrollPosition -= amount;
 					var newHorzScrollPos:Number = controller.horizontalScrollPosition;
 					if (oldHorzScrollPos == newHorzScrollPos) {
						nextLine = textFlow.flowComposer.numLines - 1;
						nextTextFlowLine = textFlow.flowComposer.getLineAt(nextLine);													
 					} else {
 						nextLine = curLine;
 						while (nextLine < (textFlow.flowComposer.numLines - 1))
 						{
 							nextLine++;
							nextTextFlowLine = textFlow.flowComposer.getLineAt(nextLine);
							if ((curTextFlowLine.x - nextTextFlowLine.x) >= (oldHorzScrollPos - newHorzScrollPos))
								break;
 						}
 					}
 				} 					
 			}
 			else
 			{
				var contentHeight:Number = controller.contentHeight;
 				if ((controller.verticalScrollPosition + amount) > contentHeight)
 				{
 					controller.verticalScrollPosition = contentHeight;
					nextLine = textFlow.flowComposer.numLines - 1;
					nextTextFlowLine = textFlow.flowComposer.getLineAt(nextLine);								 						
 				} else
 				{
 					var oldVertScrollPos:Number = controller.verticalScrollPosition;
 					controller.verticalScrollPosition += amount;
 					var newVertScrollPos:Number = controller.verticalScrollPosition;
 					if (newVertScrollPos == oldVertScrollPos) {
						nextLine = textFlow.flowComposer.numLines - 1;
						nextTextFlowLine = textFlow.flowComposer.getLineAt(nextLine);								 											
 					} else {
 						nextLine = curLine;
 						while (nextLine < (textFlow.flowComposer.numLines - 1))
 						{
 							nextLine++;
							nextTextFlowLine = textFlow.flowComposer.getLineAt(nextLine);
							if ((nextTextFlowLine.y - curTextFlowLine.y) >= (newVertScrollPos - oldVertScrollPos))
								break;
 						} 						
 					}
 				}
 			}
 			
 			endIdx = nextTextFlowLine.absoluteStart + linePos; 
			var nextLineEnd:int = nextTextFlowLine.absoluteStart + nextTextFlowLine.textLength - 1;
			if (endIdx > nextLineEnd)
			{
				endIdx = nextLineEnd;
			}
			
			if (!extendSelection)
				begIdx = endIdx;							
			if (begIdx == endIdx)
			{
				begIdx = updateEndIfInReadOnlyElement(textFlow, begIdx);
				endIdx = updateStartIfInReadOnlyElement(textFlow, endIdx);
			} else {
				endIdx = updateStartIfInReadOnlyElement(textFlow, endIdx);
			}
			
			return range.updateRange(begIdx,endIdx);				
		 }
		  
		/**
		 * Sets the TextRange up one page.
		 * @param extendSelection	Indicates that only activeIndex should move
		 * @return true if selection changed.
		 * @playerversion Flash 10
		 * @playerversion AIR 1.5
		 * @langversion 3.0
		 */		 		 		 		 		 		 		 		 
		static public function previousPage(range:TextRange, extendSelection:Boolean = false):Boolean
		{
			if (!validateTextRange(range))
				return false;
				
			var textFlow:TextFlow = range.textFlow;
			
			var controllerIndex:int = textFlow.flowComposer.findControllerIndexAtPosition(range.activePosition);
			var controller:ContainerController = textFlow.flowComposer.getControllerAt(controllerIndex);
			
			// first line in container
			var controllerFirstLine:TextFlowLine = textFlow.flowComposer.findLineAtPosition(controller.absoluteStart);

			// if on the first line of a controller go to the beginning of the previous controller
			if (range.activePosition <= controller.absoluteStart+controllerFirstLine.textLength)
			{
				if (controllerIndex == 0)
					return false;
				range.activePosition = textFlow.flowComposer.getControllerAt(controllerIndex-1).absoluteStart;
				if (!extendSelection)
					range.anchorPosition = range.activePosition;
				return true;
			}
			
			// if not the last container go to the beginning of the current container
			if (controllerIndex != textFlow.flowComposer.numControllers-1)
			{
				range.activePosition = controller.absoluteStart;
				if (!extendSelection)
					range.anchorPosition = range.activePosition;
				return true;
			}
				
			if (!isScrollable(textFlow, range.activePosition))		// paging applies only to containers that scroll
				return false;
								
			if (adjustForOversetBack(range))
				return true;
			
			var begIdx:int = range.absoluteStart;
			var endIdx:int = range.absoluteEnd;
			var curLine:int = textFlow.flowComposer.findLineIndexAtPosition(endIdx);
			var curTextFlowLine:TextFlowLine = textFlow.flowComposer.getLineAt(curLine);
			
			var lineStart:int = textFlow.flowComposer.getLineAt(curLine).absoluteStart;
			var linePos:int = endIdx - lineStart;
 			var nextLine:int;
 			var nextTextFlowLine:TextFlowLine = curTextFlowLine;
 											
			var isTTB:Boolean = textFlow.computedFormat.blockProgression == BlockProgression.RL;
			var amount:Number;
			
			// get the last container
			controller = textFlow.flowComposer.getControllerAt(textFlow.flowComposer.numControllers-1);
			
			if (isTTB)
			{
				amount = controller.compositionWidth * textFlow.configuration.scrollPagePercentage;
			} else {
				amount = controller.compositionHeight * textFlow.configuration.scrollPagePercentage;
			}
  								
 			if (isTTB)
 			{
 				if ((controller.horizontalScrollPosition + amount + controller.compositionWidth) > 0)
 				{
 					controller.horizontalScrollPosition = 0;
					nextLine = textFlow.flowComposer.findLineIndexAtPosition(controller.absoluteStart);
					nextTextFlowLine = textFlow.flowComposer.getLineAt(nextLine);								
 				} else
 				{
 					var oldHorzPos:Number = controller.horizontalScrollPosition;
 					controller.horizontalScrollPosition += amount;
 					var newHorzPos:Number = controller.horizontalScrollPosition;
 					if (oldHorzPos == newHorzPos) {
						nextLine = textFlow.flowComposer.findLineIndexAtPosition(controller.absoluteStart);
						nextTextFlowLine = textFlow.flowComposer.getLineAt(nextLine);													
 					} else {
 						nextLine = curLine;
 						while (nextLine > 0)
 						{
 							nextLine--;
							nextTextFlowLine = textFlow.flowComposer.getLineAt(nextLine);
							if ((nextTextFlowLine.x - curTextFlowLine.x) >= (newHorzPos - oldHorzPos) || nextTextFlowLine.absoluteStart < controller.absoluteStart)
								break;
 						}
 					}
 				} 					
 			}
 			else
 			{
 				if ((controller.verticalScrollPosition - amount + controller.compositionHeight) < 0)
 				{
 					controller.verticalScrollPosition = 0;
					nextLine = textFlow.flowComposer.findLineIndexAtPosition(controller.absoluteStart);
					nextTextFlowLine = textFlow.flowComposer.getLineAt(nextLine);								 						
 				} else
 				{
 					var oldVertPos:Number = controller.verticalScrollPosition;
 					controller.verticalScrollPosition -= amount;
 					var newVertPos:Number = controller.verticalScrollPosition;
 					if (oldVertPos == newVertPos) {
						nextLine = textFlow.flowComposer.findLineIndexAtPosition(controller.absoluteStart);
						nextTextFlowLine = textFlow.flowComposer.getLineAt(nextLine);								 											
 					} else {
 						nextLine = curLine;
 						while (nextLine > 0)
 						{
 							nextLine--;
							nextTextFlowLine = textFlow.flowComposer.getLineAt(nextLine);
							if ((curTextFlowLine.y - nextTextFlowLine.y) >= (oldVertPos - newVertPos) || nextTextFlowLine.absoluteStart < controller.absoluteStart)
								break;
 						}
 					} 						
 				}
 			}
 			
 			endIdx = nextTextFlowLine.absoluteStart + linePos; 
			var nextLineEnd:int = nextTextFlowLine.absoluteStart + nextTextFlowLine.textLength - 1;
			if (endIdx > nextLineEnd)
			{
				endIdx = nextLineEnd;
			}
			
			if (!extendSelection)
				begIdx = endIdx;							
			if (begIdx == endIdx)
			{
				begIdx = updateEndIfInReadOnlyElement(textFlow, begIdx);
				endIdx = updateStartIfInReadOnlyElement(textFlow, endIdx);
			} else {
				endIdx = updateStartIfInReadOnlyElement(textFlow, endIdx);
			}
			return range.updateRange(begIdx,endIdx);						
		} 
		/**
		 * Sets the TextRange at the end of the line.
		 * @param extendSelection	Indicates that only activeIndex should move
		 * @return true if selection changed.
		 * @playerversion Flash 10
		 * @playerversion AIR 1.5
		 * @langversion 3.0
		 */		 		 		 		 		 		 		 		 		 
		static public function endOfLine(range:TextRange, extendSelection:Boolean = false):Boolean
		{
			if (!validateTextRange(range))
				return false;
			

			var textFlow:TextFlow = range.textFlow;
			checkCompose(textFlow.flowComposer, range.absoluteEnd);

			var begIdx:int = range.anchorPosition;
			var endIdx:int = range.activePosition;
			var curLine:int = textFlow.flowComposer.findLineIndexAtPosition(endIdx);
			var lineStart:int = textFlow.flowComposer.getLineAt(curLine).absoluteStart;
			var lineEnd:int = lineStart + textFlow.flowComposer.getLineAt(curLine).textLength - 1;
			
			var leaf:FlowLeafElement = textFlow.findLeaf(endIdx);
			var para:ParagraphElement = leaf.getParagraph();
			if (CharacterUtil.isWhitespace(para.getCharCodeAtPosition(lineEnd - para.getAbsoluteStart())))
			{
				endIdx = lineEnd;
			} else {
				endIdx = lineEnd + 1;
			}
			if (!extendSelection)
				begIdx = endIdx;							
			if (begIdx == endIdx)
			{
				begIdx = updateEndIfInReadOnlyElement(textFlow, begIdx);
				endIdx = updateStartIfInReadOnlyElement(textFlow, endIdx);
			} else {
				endIdx = updateStartIfInReadOnlyElement(textFlow, endIdx);
			}
			return range.updateRange(begIdx,endIdx);
		 }
		 
		/**
		 * Sets the TextRange at the beginning of the line.
		 * @param extendSelection	Indicates that only activeIndex should move
		 * @return true if selection changed.
		 * @playerversion Flash 10
		 * @playerversion AIR 1.5
		 * @langversion 3.0
		 */		 		 		 		 		 		 		 		 		 		 
		static public function startOfLine(range:TextRange, extendSelection:Boolean = false):Boolean
		{
			if (!validateTextRange(range))
				return false;
				
			var textFlow:TextFlow = range.textFlow;
			checkCompose(textFlow.flowComposer, range.absoluteEnd);
			var begIdx:int = range.anchorPosition;
			var endIdx:int = range.activePosition;
		
			var curLine:int = textFlow.flowComposer.findLineIndexAtPosition(endIdx);
			var lineStart:int = textFlow.flowComposer.getLineAt(curLine).absoluteStart;
			endIdx = lineStart;
			if (!extendSelection)
				begIdx = endIdx;							
			if (begIdx == endIdx)
			{
				begIdx = updateEndIfInReadOnlyElement(textFlow, begIdx);
				endIdx = updateStartIfInReadOnlyElement(textFlow, endIdx);
			} else {
				endIdx = updateStartIfInReadOnlyElement(textFlow, endIdx);
			}
			return range.updateRange(begIdx,endIdx);
		} 
		/**
		 * Sets the TextRange at the end of the document.
		 * @param extendSelection	Indicates that only activeIndex should move
		 * @return true if selection changed.
		 * @playerversion Flash 10
		 * @playerversion AIR 1.5
		 * @langversion 3.0
		 */		 		 		 		 		 		 		 		 		 		 		 
		static public function endOfDocument(range:TextRange, extendSelection:Boolean = false):Boolean
		{
			if (!validateTextRange(range))
				return false;
				
			var textFlow:TextFlow = range.textFlow
			var begIdx:int = range.anchorPosition;
			var endIdx:int = range.activePosition;
			endIdx = textFlow.textLength;
			if (!extendSelection)
				begIdx = endIdx;							
			if (begIdx == endIdx)
			{
				begIdx = updateEndIfInReadOnlyElement(textFlow, begIdx);
				endIdx = updateStartIfInReadOnlyElement(textFlow, endIdx);
			} else {
				endIdx = updateStartIfInReadOnlyElement(textFlow, endIdx);
			}
			return range.updateRange(begIdx,endIdx);				
		}
		
		/**
		 * Sets the TextRange at the beginning of the document.
		 * @param extendSelection	Indicates that only activeIndex should move
		 * @return true if selection changed.
		 * @playerversion Flash 10
		 * @playerversion AIR 1.5
		 * @langversion 3.0
		 */		 		 		 		 		 		 		 		 		 		 		 		 
		static public function startOfDocument(range:TextRange, extendSelection:Boolean = false):Boolean
		{
			var begIdx:int = range.anchorPosition;
			var endIdx:int = 0;

			if (!extendSelection)
				begIdx = endIdx;							
			if (begIdx == endIdx)
			{
				begIdx = updateEndIfInReadOnlyElement(range.textFlow, begIdx);
				endIdx = updateStartIfInReadOnlyElement(range.textFlow, endIdx);
			} else {
				endIdx = updateStartIfInReadOnlyElement(range.textFlow, endIdx);
			}
			return range.updateRange(begIdx,endIdx);				
		}
		 
		/**
		 * Sets the TextRange at the beginning of the paragraph.
		 * @param extendSelection	Indicates that only activeIndex should move
		 * @return true if selection changed.
		 * @playerversion Flash 10
		 * @playerversion AIR 1.5
		 * @langversion 3.0
		 */		 		 		 		 		 		 		 		 		 		 		 		 		 
		static public function startOfParagraph(range:TextRange, extendSelection:Boolean = false):Boolean
		{
			var begIdx:int = range.anchorPosition;
			var endIdx:int = range.activePosition;
			var leaf:FlowLeafElement = range.textFlow.findLeaf(endIdx);
			var para:ParagraphElement = leaf.getParagraph();
			
			endIdx = para.getAbsoluteStart();
			if (!extendSelection)
				begIdx = endIdx;							

			if (begIdx == endIdx)
			{
				begIdx = updateStartIfInReadOnlyElement(range.textFlow, begIdx);
				endIdx = updateEndIfInReadOnlyElement(range.textFlow, endIdx);
			} else {
				endIdx = updateEndIfInReadOnlyElement(range.textFlow, endIdx);
			}
			return range.updateRange(begIdx,endIdx);
		 }
		 
		/**
		 * Sets the TextRange at the end of the paragraph.
		 * @param extendSelection	Indicates that only activeIndex should move
		 * @return true if selection changed.
		 * @playerversion Flash 10
		 * @playerversion AIR 1.5
		 * @langversion 3.0
		 */		 		 		 		 		 		 		 		 		 		 		 		 		 		 
		static public function endOfParagraph(range:TextRange, extendSelection:Boolean = false):Boolean
		{
			if (!validateTextRange(range))
				return false;
				
			var begIdx:int = range.anchorPosition;
			var endIdx:int = range.activePosition;
			var leaf:FlowLeafElement = range.textFlow.findLeaf(endIdx);
			var para:ParagraphElement = leaf.getParagraph();
			
			endIdx = para.getAbsoluteStart() + para.textLength - 1;
			if (!extendSelection)
				begIdx = endIdx;							
			if (begIdx == endIdx)
			{
				begIdx = updateStartIfInReadOnlyElement(range.textFlow, begIdx);
				endIdx = updateEndIfInReadOnlyElement(range.textFlow, endIdx);
			} else {
				endIdx = updateEndIfInReadOnlyElement(range.textFlow, endIdx);
			}
			return range.updateRange(begIdx,endIdx);
		 }
	
		/** If the range is in overset text (after the last container in a non-scrolling flow), adjust the range so it is at the end of the flow. */
		static private function adjustForOversetForward(range:TextRange):Boolean
		{
			var flowComposer:IFlowComposer = range.textFlow.flowComposer;
			var controller:ContainerController = null;
			checkCompose(flowComposer, range.absoluteEnd);
			if (range.absoluteEnd > flowComposer.damageAbsoluteStart - 1)
			{
				clampToFit(range, flowComposer.damageAbsoluteStart - 1);
				return true;
			}
			if (flowComposer && flowComposer.numControllers)
			{
				var controllerIndex:int = flowComposer.findControllerIndexAtPosition(range.absoluteEnd);
				if (controllerIndex >= 0)
					controller = flowComposer.getControllerAt(controllerIndex);
				if (controllerIndex == flowComposer.numControllers-1)
				{
					if (controller.absoluteStart + controller.textLength <= range.absoluteEnd && controller.absoluteStart + controller.textLength != range.textFlow.textLength)
						controller = null;
				}
			}

			if (!controller)		// we're overset, or one position before overset
			{
				range.anchorPosition = range.textFlow.textLength;
				range.activePosition = range.anchorPosition;
				return true;
			}
			return false;
		}
		
		static private function clampToFit(range:TextRange, endPos:int):void
		{
			if (endPos < 0)
				endPos = 0;
			range.anchorPosition = Math.min(range.anchorPosition, endPos);
			range.activePosition = Math.min(range.activePosition, endPos); 
		}
		
		/** If the range is in overset text (after the last container in a non-scrolling flow), adjust the range so it is at the end of the last controller in the flow. */
		static private function adjustForOversetBack(range:TextRange):Boolean
		{
			var flowComposer:IFlowComposer = range.textFlow.flowComposer;
			if (flowComposer)
			{
				checkCompose(flowComposer, range.absoluteEnd);
				if (range.absoluteEnd > flowComposer.damageAbsoluteStart - 1)
				{
					clampToFit(range, flowComposer.damageAbsoluteStart - 1);
					return true;
				} 
				if (flowComposer.findControllerIndexAtPosition(range.absoluteEnd) == -1)
				{
					range.anchorPosition = endOfLastController(range.textFlow);
					range.activePosition = range.anchorPosition;
					return true;
				}				
			}
				
			return false;
		}
		
		private static function checkCompose(flowComposer:IFlowComposer, pos:int):void
		{
			if (flowComposer.damageAbsoluteStart <= pos)
				flowComposer.composeToPosition(pos);
		}
		
		// Returns absolute position of the last controller in the flow, or 0 if the flow has no controllers
		private static function endOfLastController(flowRoot:TextFlow):int
		{
			var flowComposer:IFlowComposer = flowRoot.flowComposer;
			if (!flowComposer || flowComposer.numControllers <= 0)
				return 0;
				
			var controller:ContainerController = flowComposer.getControllerAt(flowComposer.numControllers - 1);
			return controller.absoluteStart + Math.max(controller.textLength - 1, 0);
		}
		
		// Returns true if the position is in the overset text after the last container in the flow.
		private static function isOverset(flowRoot:TextFlow, absolutePos:int):Boolean
		{
			var flowComposer:IFlowComposer = flowRoot.flowComposer;
			return (!flowComposer || flowComposer.findControllerIndexAtPosition(absolutePos) == -1);
		}
	
		// Returns true if the position is in a scollable container
		private static function isScrollable(flowRoot:TextFlow, absolutePos:int):Boolean
		{
			var flowComposer:IFlowComposer = flowRoot.flowComposer;
			if (!flowComposer)
				return false;
			var controllerIndex:int = flowComposer.findControllerIndexAtPosition(absolutePos);
			if (controllerIndex >= 0)
			{
				var controller:ContainerController = flowComposer.getControllerAt(controllerIndex);
				var blockProgression:String = controller.rootElement.computedFormat.blockProgression;
				return ((blockProgression == BlockProgression.TB && controller.verticalScrollPolicy != ScrollPolicy.OFF) ||
					(blockProgression == BlockProgression.RL && controller.horizontalScrollPolicy != ScrollPolicy.OFF));
			}
			return false;
		}
		
	}
}
