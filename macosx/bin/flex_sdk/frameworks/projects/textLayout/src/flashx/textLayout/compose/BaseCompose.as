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
package flashx.textLayout.compose
{
	import flash.display.DisplayObject;
	import flash.geom.Rectangle;
	import flash.text.engine.TextBaseline;
	import flash.text.engine.TextBlock;
	import flash.text.engine.TextElement;
	import flash.text.engine.TextLine;
	import flash.text.engine.TextLineCreationResult;
	import flash.text.engine.TextLineValidity;
	
	import flashx.textLayout.container.ContainerController;
	import flashx.textLayout.debug.Debugging;
	import flashx.textLayout.debug.assert;
	import flashx.textLayout.elements.Configuration;
	import flashx.textLayout.elements.ContainerFormattedElement;
	import flashx.textLayout.elements.FlowElement;
	import flashx.textLayout.elements.FlowGroupElement;
	import flashx.textLayout.elements.FlowLeafElement;
	import flashx.textLayout.elements.InlineGraphicElement;
	import flashx.textLayout.elements.LinkElement;
	import flashx.textLayout.elements.ListElement;
	import flashx.textLayout.elements.ListItemElement;
	import flashx.textLayout.elements.OverflowPolicy;
	import flashx.textLayout.elements.ParagraphElement;
	import flashx.textLayout.elements.TCYElement;
	import flashx.textLayout.elements.TextFlow;
	import flashx.textLayout.formats.BaselineOffset;
	import flashx.textLayout.formats.BlockProgression;
	import flashx.textLayout.formats.ClearFloats;
	import flashx.textLayout.formats.Direction;
	import flashx.textLayout.formats.Float;
	import flashx.textLayout.formats.FormatValue;
	import flashx.textLayout.formats.IListMarkerFormat;
	import flashx.textLayout.formats.ITextLayoutFormat;
	import flashx.textLayout.formats.LeadingModel;
	import flashx.textLayout.formats.ListStylePosition;
	import flashx.textLayout.formats.TextAlign;
	import flashx.textLayout.formats.TextLayoutFormat;
	import flashx.textLayout.formats.VerticalAlign;
	import flashx.textLayout.tlf_internal;
	import flashx.textLayout.utils.LocaleUtil;
	import flashx.textLayout.utils.Twips;

	use namespace tlf_internal;
	
	
	[ExcludeClass]
	/** @private Common composer base class */
	public class BaseCompose
	{
		
		public static function get globalSWFContext():ISWFContext
		{ 
			return GlobalSWFContext.globalSWFContext; 
		}
		
		// a one deep AlignData cache
		static private var _savedAlignData:AlignData;
		static private function createAlignData(tfl:TextFlowLine):AlignData
		{ 
			if (_savedAlignData)
			{
				var rslt:AlignData = _savedAlignData; 
				rslt.textFlowLine = tfl;
				_savedAlignData = null;
				return rslt;
			}
			return new AlignData(tfl);
		}
		static private function releaseAlignData(ad:AlignData):void
		{ 
			ad.textLine = null;	// don't hold on to it
			ad.textFlowLine = null;
			_savedAlignData = ad; 
		}
		
		protected var _parcelList:ParcelList;
		
		/** List of areas we're composing into, matches the container's bounding box */
		public function get parcelList():ParcelList
		{ return _parcelList; }
		
		/** Element of current location */
		protected var _curElement:FlowLeafElement;		
		/** Absolute start position of _curElement */
		protected var _curElementStart:int;		
		/** Offset from element start to current location */
		protected var _curElementOffset:int;		
		/** ParagraphElement that contains the current location */
		protected var _curParaElement:ParagraphElement;	
		protected var _curParaFormat:ITextLayoutFormat;
		/** Absolute start position of _curParaElement */
		protected var _curParaStart:int;
		/** leading model for the current line's para (set when line is being composed and committed to _lastLineLeadingModel when line is finalized) */
		private var _curLineLeadingModel:String = "";
		/** leading factor calculated when composing the current line (committed to _lastLineLeading when line is finalized)
		 * The factor has different meanings for different leading models. */
		private var _curLineLeading:Number;
		/** leading model for the last line's para */
		protected var _lastLineLeadingModel:String = "";
		/** leading factor calculated for the line composed last. 
		 * The factor has different meanings for different leading models. */
		protected var _lastLineLeading:Number;
		/** effective descent of the last line. This is the distance between a line's Roman baseline and 
		 * its Descent baseline (for leading models other than LeadingModel.BOX) or the bottom of its CSS line box (for LeadingModel.BOX) */
		protected var _lastLineDescent:Number;
		/** Amount of spaceAfter added to the previous line (used in calculating effective paragraphSpaceBefore/paragraphSpaceAfter */
		protected var _paragraphSpaceCarried:Number;
		/** Amount of vertical space added to the previous element (used in calculating vertical space collapse) **/
		protected var _verticalSpaceCarried:Number;
		/** BlockProgression - vertical horizontal etc. @see text.formats.BlockProgression */
		protected var _blockProgression:String;
		/** Are we at the top of the column? */
		protected var _atColumnStart:Boolean;
		/** Current textIndent amount, 0 if line is not the first */
		protected var _textIndent:Number;
		
		/** Minimum left edge coordinate across all the parcels in a controller */
		private var _controllerLeft:Number;
		/** Minimum top edge across all the parcels in a controller */
		private var _controllerTop:Number;
		/** Maximum right edge coordinate across all the parcels in a controller */
		private var _controllerRight:Number;
		/** Maximum bottom edge coordinate across all the parcels in a controller */
		private var _controllerBottom:Number;
		
		/** Maximum horizontal extension from left/right edge of the parcel.  Alignment width for the parcel when measuring. */
		protected var _contentLogicalExtent:Number;
		/** Commited extent any lines needing additional alignment must update this number */
		protected var _contentCommittedExtent:Number;
		/** Committed logical height from floats */
		protected var _contentCommittedHeight:Number;
		/** Maximum horizontal extension for the current line from left/right edge of the parcel.  */
		protected var _workingContentLogicalExtent:Number;
		/* Extent for the current line, accumulated into _contentCommitedExtent when the line is committed */
		protected var _workingContentExtent:Number;
		/* Height for the current line, accumulated into _contentCommittedHeight when the line is committed */
		protected var _workingContentHeight:Number;
		/* Total depth as of the start of the line */
		protected var _workingTotalDepth:Number;
		/* Current parcel when line was started */
		protected var _workingParcelIndex:int;
		/** logical top of the parcel, for the line in progress */
		protected var _workingParcelLogicalTop:Number;
		/** Minimum starting coord for parcel bounds */
		protected var _accumulatedMinimumStart:Number;
		/** Minimum starting coord for parcel bounds */
		protected var _parcelLogicalTop:Number;
		
		/** Minimum left edge coordinate across all the parcels in a controller */
		protected var _parcelLeft:Number;
		/** Minimum top edge across all the parcels in a controller */
		protected var _parcelTop:Number;
		/** Maximum right edge coordinate across all the parcels in a controller */
		protected var _parcelRight:Number;
		/** Maximum bottom edge coordinate across all the parcels in a controller */
		protected var _parcelBottom:Number;
		
		/** owning textFlow of current compose */
		protected var _textFlow:TextFlow;
		private var _releaseLineCreationData:Boolean;
		/** flowComposer of current compose */
		protected var _flowComposer:IFlowComposer;
		/** rootElement of current compose */
		protected var _rootElement:ContainerFormattedElement;
		/** position to stop composing at */
		protected var _stopComposePos:int;
		
		/** First damaged controller to begin composing */
		protected var _startController:ContainerController;
		/** Beginning composition position.  Note this gets cleared once its been passed */
		protected var _startComposePosition:int;
		
		/** Visible area of the current controller. Used to determine which lines are going to be in view. */	
		protected var _controllerVisibleBoundsXTW:int;
		protected var _controllerVisibleBoundsYTW:int;
		protected var _controllerVisibleBoundsWidthTW:int;
		protected var _controllerVisibleBoundsHeightTW:int; 
		
		protected var _forceILGs:Boolean;
		protected var _lastGoodStart:int;		// used to group lines together in a single container when composing floats
		protected var _linePass:int;			// count of how many times we've retried this line
		protected var _paragraphContainsVisibleLines:Boolean;
		
		// save line slug for reuse - otherwise we're creating and discarding all the time
		static protected var _savedLineSlug:Slug;
		protected var _lineSlug:Slug = new Slug();
		static protected var _floatSlug:Slug;
		protected var _pushInFloats:Array;
		
		// scratch array for holding lines awaiting alignment
		private var _alignLines:Array;
		
		/** Parcel we are composing - used for keeping track of when it changes b/c parcelList.parcel may have advanced */
		protected var _curParcel:Parcel;
		
		/** Start position of _curParcel */
		protected var _curParcelStart:int;
		
		/** Are we measuring */
		protected var _measuring:Boolean;
		
		protected var _curLine:TextFlowLine;
		protected var _previousLine:TextLine;
		
		protected var _listItemElement:ListItemElement;
		
		protected function createParcelList():ParcelList
		{ return null; }
		protected function releaseParcelList(list:ParcelList):void	// No PMD
		{ }
		
		/** Starting controller for skipping ahead */
		public function get startController():ContainerController
		{ return _startController; }
		
		/** prevent any leaks. @private */
		tlf_internal function releaseAnyReferences():void
		{
			_curElement = null;
			_curParaElement = null;
			_curParaFormat = null;
			_flowComposer = null;
			_parcelList = null;
			_rootElement = null;
			_startController = null;
			_textFlow = null;
			_previousLine = null;
			_curLine = null;
		}
		
		
		/** Initialize for a composition that will compose up through the controllerEndIndex, or all the way to the end of the flow
		 * @param composer
		 * @param composeToPosition 	-1 means not specified.  0 means request to compose nothing, >0 specifies a position to force compose to
		 * @param controllerStartIndex	index of the first controller to compose for, derived class allows -1 for default 
		 * @param controllerEndIndex	index of the last controller to compose for, or -1 to compose through all controllers
		 */
		protected function  initializeForComposer(composer:IFlowComposer, composeToPosition:int, controllerStartIndex:int, controllerEndIndex:int):void
		{
			CONFIG::debug 
			{ 
				var count:int = 0;
				_textFlow.applyFunctionToElements(function (elem:FlowElement):Boolean{ if (elem.defaultTypeName == "img") count++; return false; });
				assert(count == _textFlow.graphicObjectCount,"BaseCompose mistmatched graphicObject count");
				
				count = 0;
				_textFlow.applyFunctionToElements(function (elem:FlowElement):Boolean{ if (elem.hasActiveEventMirror()) count++; if (elem is LinkElement) count++; return false; });
				assert(count == _textFlow.interactiveObjectCount,"BaseCompose mistmatched interactiveObject count");

				assert(composer.getControllerIndex(_startController) == controllerStartIndex, "BaseCompose initializeForComposer expected derived class to set up _startController before now"); 
				assert(controllerStartIndex >= 0, "BaseCompose initializeForComposer expected derived class to set controllerStartIndex to a valid controller"); 
			}
			if (!_savedLineSlug)
				_lineSlug = new Slug();
			else
			{
				_lineSlug = _savedLineSlug;
				_savedLineSlug = null;
			}
			_parcelList = createParcelList();
			
			_paragraphSpaceCarried = 0;
			_blockProgression = composer.rootElement.computedFormat.blockProgression;

			// for a non-specified compose position the ParcelList handles the bail out - just set to textLength
			_stopComposePos = composeToPosition >= 0 ? Math.min(_textFlow.textLength,composeToPosition) : _textFlow.textLength;
			
			if (controllerStartIndex < 0)
				controllerStartIndex = 0;
			
			// this chains through the list - tell it if a "care about" comopseToPosition was specified
			_parcelList.beginCompose(composer, controllerStartIndex, controllerEndIndex, composeToPosition > 0);	
			
			_contentLogicalExtent = 0;
			_contentCommittedExtent = 0;
			_contentCommittedHeight = 0;
			_accumulatedMinimumStart = TextLine.MAX_LINE_WIDTH;
			_parcelLogicalTop = NaN;
			
			_linePass = 0;
			_lastGoodStart = -1;
			if (_pushInFloats)
				_pushInFloats.length = 0;
			
			_listItemElement = null;
		}
		
		private function composeBlockElement(elem:FlowGroupElement,absStart:int):Boolean
		{	
			var child:FlowElement;
			var rslt:Boolean;	// scratch
			
			// Iterate through the children, composing them. If we're starting in the middle of the element,
			// make sure we advance to the starting child.
			var idx:int = 0;
			if (absStart != _curElementStart + _curElementOffset) 	// starting partway in
			{
				idx = elem.findChildIndexAtPosition((_curElementStart + _curElementOffset) - absStart);
				child = elem.getChildAt(idx);
				absStart += child.parentRelativeStart;
			}

			var composeEntireElement:Boolean = (absStart == _curElementStart + _curElementOffset);

			// Compose all the children, until all the containers are filled, or if we're on the last container, we've hit the stop compose text index			
			for (; idx < elem.numChildren && (absStart <= _stopComposePos || ! parcelList.atLast()); idx++)
			{
				child = elem.getChildAt(idx);
				
				// If the element has clear applied, handle that now
				if (child.computedFormat.clearFloats != ClearFloats.NONE)
				{
					var adjustedDepth:Number = _curParcel.applyClear(child.computedFormat.clearFloats, _parcelList.totalDepth, child.computedFormat.direction);
					_parcelList.addTotalDepth(adjustedDepth);
					_verticalSpaceCarried = 0;
				}

				var boxLeftIndent:Number;		// logical with respect to horizontal/vertical text
				var boxRightIndent:Number;		// logical with respect to horizontal/vertical text
				var boxTopIndent:Number;		// logical with respect to horizontal/vertical text
				var boxBottomIndent:Number;		// logical with respect to horizontal/vertical text
				if (_blockProgression == BlockProgression.RL)
				{
					boxLeftIndent = child.getEffectivePaddingTop();
					boxRightIndent = child.getEffectivePaddingBottom();
					boxTopIndent = child.getEffectivePaddingRight();
					boxBottomIndent = child.getEffectivePaddingLeft();
				}
				else
				{
					boxLeftIndent = child.getEffectivePaddingLeft();
					boxRightIndent = child.getEffectivePaddingRight();
					boxTopIndent = child.getEffectivePaddingTop();
					boxBottomIndent = child.getEffectivePaddingBottom();
				}
				CONFIG::debug { assert(!isNaN(boxLeftIndent) && ! isNaN(boxRightIndent),"BAD indents"); }
				_parcelList.pushLeftMargin(boxLeftIndent);
				_parcelList.pushRightMargin(boxRightIndent);
				if (composeEntireElement && boxTopIndent > _verticalSpaceCarried)
					_parcelList.addTotalDepth(boxTopIndent - _verticalSpaceCarried);
				_verticalSpaceCarried = Math.max(boxTopIndent, 0);
								
				var para:ParagraphElement = child as ParagraphElement;
				if (para)
				{
					if (!composeParagraphElement(para,absStart))
						return false;	// done
				}
				else if (child is ListElement)
				{						
					rslt = composeBlockElement(FlowGroupElement(child),absStart);
					
					if (!rslt)
						return false;
				}
				else if (child is ListItemElement)
				{
					var savedListItemElement:ListItemElement = _listItemElement;
					_listItemElement = child as ListItemElement;
					rslt = composeBlockElement(FlowGroupElement(child),absStart);
					_listItemElement = savedListItemElement;
						
					if (!rslt)
						return false;
				}
				else 
				{
					if (!composeBlockElement(FlowGroupElement(child),absStart))
						return false;
				}
				
				if (boxBottomIndent > _verticalSpaceCarried)
					_parcelList.addTotalDepth(boxBottomIndent - _verticalSpaceCarried);
				_verticalSpaceCarried = Math.max(boxBottomIndent, 0);

				// restore to original values
				_parcelList.popLeftMargin(boxLeftIndent);
				_parcelList.popRightMargin(boxRightIndent);
				
				absStart += child.textLength;
				composeEntireElement = true;
			}
			return true;
		}
		
		/**
		 * Compose the flow into the text container. Starts at the root element,
		 * and composes elements until either there are no more elements, or the
		 * text container is full. It will compose only the lines which are
		 * marked invalid, so that existing lines that are unchanged are not
		 * recomposed.
		 */
		public function composeTextFlow(textFlow:TextFlow, composeToPosition:int, controllerEndIndex:int):int
		{
			_textFlow = textFlow;
			_releaseLineCreationData = textFlow.configuration.releaseLineCreationData && Configuration.playerEnablesArgoFeatures;
			
			_flowComposer = _textFlow.flowComposer;
			_rootElement = textFlow;
			_curElementOffset = 0;
			_curElement = _rootElement.getFirstLeaf();	
			
			_curElementStart = 0;		// current position in the text (start of current line)
			
			_curParcel = null;

			// must setup _startController and _startComposePosition
			initializeForComposer(_flowComposer, composeToPosition, -1 /* use default */, controllerEndIndex);
			
			resetControllerBounds();
			
			// This is where we will start composing from
			_curElement = _textFlow.findLeaf(_startComposePosition);
			_curElementStart = _curElement.getAbsoluteStart();
			_curElementOffset = _startComposePosition - _curElementStart;

			// If we're starting composition from the middle of the container, we have to set up the composition
			// state so that its the same as if we'd composed from the start.
			if (_startComposePosition <= _startController.absoluteStart || !advanceToComposeStartPosition())
			{
				if (_startComposePosition > _startController.absoluteStart)
				{
					// We tried to start from the middle, but it didn't succeed. Reset from the start of the container
					_startComposePosition = _startController.absoluteStart;
					_curElement = _textFlow.findLeaf(_startComposePosition);
					_curElementStart = _curElement.getAbsoluteStart();
					_curElementOffset = _startComposePosition - _curElementStart;
				} 
				if (_startComposePosition == _curElement.getParagraph().getAbsoluteStart())
					_previousLine = null;
				else
				{
					var startLineIndex:int = _flowComposer.findLineIndexAtPosition(_startComposePosition - 1);
					var line:TextFlowLine = _flowComposer.getLineAt(startLineIndex);
					_previousLine = line.getTextLine(true);
				}
				advanceToNextParcel();		// advance to first parcel
				if (_curParcel)
					_curParcel.controller.clearFloatsAt(0);
			}
			
			_startController.clearComposedLines(_curElementStart + _curElementOffset);
			
			_curParcelStart = _startController.absoluteStart; 		// yuck! this gets set to where we're composing from in parcelHasChanged, set it back to start of parcel
			
			composeInternal(_rootElement,0);
			
			for (;;)
			{
				if (parcelList.atEnd())
				{
					parcelHasChanged(null);		// force end of composition accounting for the parcel
					break;
				}				
				advanceToNextParcel();
			}
			releaseParcelList(_parcelList);
			_parcelList = null;
			
			_savedLineSlug = _lineSlug;
			
			return _curElementStart + _curElementOffset;		// Return last composed position
		}
		
		// If we're starting composition from the middle of the container, we have to set up the composition
		// state so that its the same as if we'd composed from the start.
		private function advanceToComposeStartPosition():Boolean
		{
			var startLineIndex:int = _flowComposer.findLineIndexAtPosition(_startComposePosition - 1);
			var curLine:TextFlowLine = _flowComposer.getLineAt(startLineIndex);
			
			// cannot start in the middle if we are measuring AND there are floats
			if (curLine.controller.numFloats)
			{
				CONFIG::debug { assert((_blockProgression == BlockProgression.TB && curLine.controller.measureWidth 
					|| _blockProgression == BlockProgression.RL && curLine.controller.measureHeight) == _measuring,"Bad _measuring intialization"); }
				if (_measuring)
					return false;
			}

			_curLine = curLine;
			var previousElement:FlowLeafElement = (_curElementOffset == 0) ? _curElement.getPreviousLeaf() : _curElement;							
			// set up previous line leading info, and paragraphSpaceCarried (in case previous line had spaceAfter).
			_curLineLeadingModel = previousElement.getParagraph().getEffectiveLeadingModel();
			var curElem:FlowLeafElement = _textFlow.findLeaf(_curLine.absoluteStart);
			var curElemStart:int = curElem.getAbsoluteStart();
			calculateLeadingParameters(curElem, curElemStart, TextFlowLine.findNumberLine(_curLine.getTextLine())); 
			if (_startComposePosition == _curElement.getParagraph().getAbsoluteStart())
				_previousLine = null;
			else
				_previousLine = _curLine.getTextLine(true);
			CONFIG::debug { assert(!_previousLine || _previousLine.userData != null, "previousLine has no user data"); }
			CONFIG::debug { assert(!_previousLine || _previousLine.validity == TextLineValidity.VALID, "preivous line is invalid"); }
			_paragraphSpaceCarried = _curLine.spaceAfter;
			commitLastLineState(_curLine);
			
			// advance through the parcels to find the column where composition will start
			var startParcel:int = _curLine.columnIndex == -1 ? 0 : _curLine.columnIndex;
			_curParcel = _parcelList.currentParcel;
			var floatIndex:int = 0;
			for (var parcelIndex:int = -1; parcelIndex < startParcel; ++parcelIndex) 	// do this at least once!
			{
				advanceToNextParcel();

				_curParcelStart = _curParcel.controller.absoluteStart;		// so that bounds will be updated in finishParcel correctly (parcel won't appear empty!)

				// Process knockouts for floats that may be in the previously composed content, so that the parcel list will have
				// whatever knockouts came before the text we're composing
				var numFloats:int = _curParcel.controller.numFloats;
				if (numFloats)
				{
					for  (; floatIndex < numFloats; ++floatIndex)
					{
						var floatInfo:FloatCompositionData = _curParcel.controller.getFloatAt(floatIndex);
						if (floatInfo.columnIndex > _curParcel.columnIndex)
							break;
						if (floatInfo.floatType != Float.NONE && floatInfo.absolutePosition < _startComposePosition)
						{
							var ilg:InlineGraphicElement = _textFlow.findLeaf(floatInfo.absolutePosition) as InlineGraphicElement;
							var logicalHeight:Number = (_blockProgression == BlockProgression.RL) ? ilg.elementWidthWithMarginsAndPadding() : ilg.elementHeightWithMarginsAndPadding();
							_curParcel.knockOut(floatInfo.knockOutWidth, floatInfo.depth - _lastLineDescent, floatInfo.depth + logicalHeight, floatInfo.floatType == Float.LEFT);
						}
					}
				}
				_curParcel.controller.clearFloatsAt(_startComposePosition);
			}
			_curParcelStart = _curElementStart + _curElementOffset;	

			// Set the depth of the parcel based on the starting line's position	
			if (_blockProgression == BlockProgression.TB)
				_parcelList.addTotalDepth(_curLine.y + _curLine.ascent - _curParcel.y);
			else
				_parcelList.addTotalDepth(_curParcel.right - _curLine.x);
			_atColumnStart = false;
			
			// generate content bounds summary for the lines from the start of the container, to the startComposePosition
			var lineIndex:int = _flowComposer.findLineIndexAtPosition(_startController.absoluteStart);
			CONFIG::debug { assert(startLineIndex + 1 == _flowComposer.findLineIndexAtPosition(_startComposePosition), "startLineIndex not as expected"); }
			initializeContentBounds(lineIndex, startLineIndex);
			
			return true;
		}
		
		// Generate content bounds for the case where we're doing incremental composition (starting compose from middle of container)
		// If we're aligning horizontally to the measured width, then any lines that are center or right will have to get
		// realigned when we're done, even if they've already been composed. But, if the line is not aligned, or if it's
		// been aligned during the previous composition to the absolute compositionWidth, then we don't have to align it again.
		private function initializeContentBounds(lineIndex:int, lastLineToCheck:int):void
		{
			var columnIndex:int = -1;
			var line:TextFlowLine;
			
			// measuring for the line widths
			CONFIG::debug { assert((_blockProgression == BlockProgression.TB && _curParcel.controller.measureWidth 
				|| _blockProgression == BlockProgression.RL && _curParcel.controller.measureHeight) == _measuring,"Bad _measuring intialization"); }
			
			_parcelLogicalTop = computeTextFlowLineMinimumLogicalTop(_flowComposer.getLineAt(lineIndex),null);
			if (_measuring)
			{
				for (; lineIndex <= lastLineToCheck; ++lineIndex)
				{
					line = _flowComposer.getLineAt(lineIndex);
					
					if (line.columnIndex != columnIndex)
					{
						columnIndex = line.columnIndex;
						_contentLogicalExtent = 0;
						_contentCommittedExtent = 0;
						_accumulatedMinimumStart = TextLine.MAX_LINE_WIDTH;
					}
					var lineExtent:Number = line.lineExtent;
					_contentLogicalExtent = Math.max(_contentLogicalExtent, lineExtent);
					var textLine:TextLine 
					// If we're aligning horizontally to the measured width, then any lines that are center or right will have to get
					// realigned when we're done, even if they've already been composed. But, if the line is not aligned, or if it's
					// been aligned during the previous composition to the absolute compositionWidth, then we don't have to align it again.

					if (line.alignment == TextAlign.LEFT && !line.hasNumberLine)
						_contentCommittedExtent = Math.max(_contentCommittedExtent, lineExtent);	
					else
					{
						var alignData:AlignData = createAlignData(line);
						alignData.textLine = line.getTextLine(true);
						alignData.textAlign = line.alignment;
						var paraFormat:ITextLayoutFormat = line.paragraph.computedFormat;
						alignData.rightSideGap = getRightSideGap(line, line.alignment != TextAlign.LEFT);
						alignData.leftSideGap = getLeftSideGap(line);
						alignData.textIndent = paraFormat.textIndent;
						alignData.lineWidth = lineExtent - (alignData.rightSideGap + alignData.leftSideGap);
						if (!_alignLines)
							_alignLines = [];
						_alignLines.push(alignData);
					}
				}	
				CONFIG::debug { assert(_flowComposer.getLineAt(lastLineToCheck).accumulatedLineExtent == _contentLogicalExtent,"Bad _contentLogicalExtent"); }
			}
			else
			{
				line = _flowComposer.getLineAt(lastLineToCheck);
				_contentLogicalExtent = _contentCommittedExtent = line.accumulatedLineExtent;
				_accumulatedMinimumStart = line.accumulatedMinimumStart;
				// Assume any columns that came before this one are full. We don't want to take the time to iterate all previous lines in previous parcels/columns.
				if (_parcelList.currentParcelIndex > 0 && _parcelList.currentParcel.columnIndex > 0)
				{
					if (_blockProgression == BlockProgression.TB)
						_controllerBottom =  _curParcel.controller.compositionHeight;
					else
						_controllerLeft = 0 - _curParcel.controller.compositionWidth;
					if (_textFlow.computedFormat.direction == Direction.RTL)		// columns are right to left
						_controllerRight = _curParcel.controller.compositionWidth;
				}
			}
		}
		
		/** @private */
		tlf_internal function computeTextFlowLineMinimumLogicalTop(line:TextFlowLine,textLine:TextLine):Number
		{
			if (line.hasGraphicElement)
			{
				var pos:int = line.absoluteStart;
				var leafElement:FlowLeafElement = _textFlow.findLeaf(pos);
				var adjustedAscent:Number = line.getLineTypographicAscent(leafElement, leafElement.getAbsoluteStart(),textLine);
				var parcelTop:Number = (_blockProgression == BlockProgression.RL) ? line.x + adjustedAscent : line.y + line.ascent - adjustedAscent;
				var controller:ContainerController = line.controller;
				var lineEnd:int = pos + line.textLength;
				if (controller.numFloats > 0)		// adjust line upwards if it has a float that came before (e.g., landed on its own line before, and there was no room for text beside)
				{
					while (pos < lineEnd)
					{
						var floatInfo:FloatCompositionData = controller.getFloatAtPosition(pos);
						if (floatInfo)
						{
							parcelTop = Math.min(parcelTop, floatInfo.depth);
							pos = floatInfo.absolutePosition + 1;
						}
						else
							break;
					}
				} 
				return parcelTop;
			}
			// don't have one
			return NaN;
		}
		
		private function resetControllerBounds():void
		{
			_controllerLeft = TextLine.MAX_LINE_WIDTH;
			_controllerTop = TextLine.MAX_LINE_WIDTH;
			_controllerRight = -TextLine.MAX_LINE_WIDTH;
			_controllerBottom = -TextLine.MAX_LINE_WIDTH;
		}
		
		/** Release line creation data during this compose */
		protected function get releaseLineCreationData():Boolean
		{ return _releaseLineCreationData; }
		
		// Create new lines through composition. lines, wrap, etc.
		protected function composeInternal(composeRoot:FlowGroupElement,absStart:int):void
		{
			composeBlockElement(composeRoot,absStart);
		}
		
		protected function composeParagraphElement(elem:ParagraphElement,absStart:int):Boolean
		{
			_curParaElement  = elem;
			_curParaStart    = absStart;
			_curParaFormat = elem.computedFormat;
			CONFIG::debug { assert(_curParaStart == elem.getAbsoluteStart(),"composeParagraphElement: bad start"); }

			// Initialize the flag so that if we are composing only part of the paragraph, we will consider it in view.
			// We could examine the earlier part of the paragraph to see if its visible, but for now just assume it
			// might be.
			_paragraphContainsVisibleLines = (_curElementStart + _curElementOffset != _curParaStart) ;
			
			var success:Boolean = composeParagraphElementIntoLines();
			
			var okToRelease:Boolean = true;

			// If no lines in the paragraph are visible, release the paragraph's TextBlock and its lines so they can be reused later
			if (!_paragraphContainsVisibleLines)
			{
				// Lines that are now composed that would not be visible on update, might still be in the display list from
				// a previous update. Don't release in that case.
				var textBlock:TextBlock = elem.getTextBlock();
				var textLine:TextLine;
				for (textLine = textBlock.lastLine; textLine && okToRelease; textLine = textLine.previousLine) 
				{
					if (textLine.parent)
						okToRelease = false;
				}
				if (okToRelease)	// no textlines were in view, go ahead and release them all, starting at the end and working to the start
				{
					for (textLine = textBlock.lastLine; textLine; )
					{
						textBlock.releaseLines(textLine, textLine);
						textLine.userData = null;
						TextLineRecycler.addLineForReuse(textLine);
						if (_textFlow.backgroundManager)
							_textFlow.backgroundManager.removeLineFromCache(textLine);
						textLine = textBlock.lastLine;
					}
					elem.releaseTextBlock();
				}
			}

			// If we didn't release the TextBlock, flush it to save memory (at the cost of performance during editing).
			// This saves a lot of memory in Argo, is a nop on older players. only newer players implement flush	
			if (releaseLineCreationData && !okToRelease)
				elem.releaseLineCreationData();

			return success;
		}
		
		protected function getFirstIndentCharPos(paragraph:ParagraphElement):int
		{
			var pos:int = 0;
			var leaf:FlowLeafElement = paragraph.getFirstLeaf();
			while (leaf && (leaf is InlineGraphicElement) && (InlineGraphicElement(leaf).effectiveFloat != Float.NONE))
			{
				pos += leaf.textLength;
				leaf = leaf.getNextLeaf();
			}
			return pos;
		}
		
		/** Compose the lines in the paragraph. Returns true if composition should continue, false if all space is used and composition should stop. */
		protected function composeParagraphElementIntoLines():Boolean
		{
			var result:Boolean = true;
			var textLine:TextLine;
			
			var leftMargin:Number;
			var rightMargin:Number;
			
			var firstLineIndent:Number = 0;
			if (_curParaFormat.direction == Direction.LTR)
			{
				leftMargin = _curParaFormat.paragraphStartIndent;
				rightMargin = _curParaFormat.paragraphEndIndent;
			}
			else
			{
				leftMargin = _curParaFormat.paragraphEndIndent;
				rightMargin = _curParaFormat.paragraphStartIndent;
			}	
			_parcelList.pushLeftMargin(leftMargin);
			_parcelList.pushRightMargin(rightMargin);
			
			var firstIndentCharPos:int = _curParaStart;
			
			if (preProcessILGs(_curElementStart - _curParaStart))
				firstIndentCharPos = getFirstIndentCharPos(_curParaElement) + _curParaStart;

			// loop creating lines_curParaStart
			while (result)
			{
				if (_parcelList.atEnd())
				{
					result = false;
					break;
				}
				
				// Allow derived classes to do processing here
				startLine();

				if (!_forceILGs)		// floats will compose as inline graphics
					processFloatsAtLineStart();
				
				_textIndent = (_curElementStart + _curElementOffset <= firstIndentCharPos) ? _curParaFormat.textIndent : 0;
				
				if (_parcelList.atEnd())
				{
					result = false;
					break;
				}

				// Get the next line
				textLine = composeNextLine();
				if (textLine ==  null)
				{
					result = false;
					break;
				}
				
				CONFIG::debug { assert(_curLine != null, "curLine is null!"); }
				
				// Adjust the coordinates of the line for center/right.  The line is always left aligned.  TextBlock handles justified cases
				// If we're on the last line of a justified paragraph, use the textAlignLast value 
				var textAlignment:String = _curParaFormat.textAlign;
				if (textAlignment == TextAlign.JUSTIFY)
				{
					var location:int = _curLine.location;
					if (location == TextFlowLineLocation.LAST || location == TextFlowLineLocation.ONLY)
						textAlignment = _curParaFormat.textAlignLast;
				}
				switch(textAlignment)
				{
					case TextAlign.START:
						textAlignment = (_curParaFormat.direction == Direction.LTR) ? TextAlign.LEFT : TextAlign.RIGHT;
						break;
					case TextAlign.END:
						textAlignment = (_curParaFormat.direction == Direction.LTR) ? TextAlign.RIGHT : TextAlign.LEFT;
						break; 
				}
				
				// need alignData whenever there is a numberLine - alignData also computes contentBounds which need to take numberlines into account
				var numberLine:TextLine = TextFlowLine.findNumberLine(textLine);
				var needAlignData:Boolean = (numberLine && TextFlowLine.getNumberLineListStylePosition(numberLine) == ListStylePosition.OUTSIDE) || textAlignment == TextAlign.CENTER || textAlignment == TextAlign.RIGHT;
				
				// in argo lines that have tabs must be either START or JUSTIFY
				if (Configuration.playerEnablesArgoFeatures)
				{
					if (textLine["hasTabs"])
					{
						if (_curParaFormat.direction == Direction.LTR)
						{
							if (!numberLine || TextFlowLine.getNumberLineListStylePosition(numberLine) == ListStylePosition.INSIDE)
								needAlignData = false;	// don't align it - let it be left align
							textAlignment = TextAlign.LEFT;
						}
						else
						{
							needAlignData = true;
							textAlignment = TextAlign.RIGHT;
						}
					}
				}

				
				var alignData:AlignData;
				if (needAlignData)
				{
					alignData = createAlignData(_curLine);
					alignData.textLine = textLine;
					alignData.textAlign = textAlignment;
				}

				/* {
				for (var idx:int = 0; idx < curLine.textLine.atomCount; idx++)
				{
				trace(idx.toString()+": beginIndex: " + curLine.textLine.getAtomTextBlockBeginIndex(idx)+ " bidiLevel: "+ curLine.textLine.getAtomBidiLevel(idx) + " bounds: " + curLine.textLine.getAtomBounds(idx));
				}
				} */
				
				// Space before does not apply to the first line, unless LeadingModel.BOX is used
				// Space carried never applies to the first line
				var spaceBefore:Number  = _atColumnStart && (_curParaFormat.leadingModel != LeadingModel.BOX) ? 0 : _curLine.spaceBefore;
				var spaceCarried:Number = _atColumnStart ? 0 : _paragraphSpaceCarried;
				if (spaceBefore != 0 || spaceCarried != 0)
					_parcelList.addTotalDepth(Math.max(spaceBefore, spaceCarried));
				
				_paragraphSpaceCarried = 0;
				if (_verticalSpaceCarried != 0)
					_verticalSpaceCarried = 0;
				_parcelList.addTotalDepth(_curLine.height);

				alignData = calculateLineAlignmentAndBounds(textLine, numberLine, alignData);
				
				if (alignData)
				{
					if (!_alignLines)
						_alignLines = [];
					_alignLines.push(alignData);
					_curLine.alignment = textAlignment;
				}
								
				// textLength is the first character in the next line
				CONFIG::debug { assert(_curParcel.controller.textLength >= 0, "frame has negative composition"); }
				
				if (firstLineIndent != 0)
				{
					if (_curParaFormat.direction == Direction.LTR)
						_parcelList.popLeftMargin(firstLineIndent);
					else
						_parcelList.popRightMargin(firstLineIndent);
					firstLineIndent = 0;
				}

				// The line didn't fit in the parcel when all the floats were factored in.
				// If we're hitting this case, we are now in the next parcel, try laying out 
				// the line there.
				if (!processFloatsAtLineEnd(textLine) || !_curLine)	
				{
					resetLine(textLine);
					continue;
				}
				
				endLine(textLine);

				_lastGoodStart = -1;		// everything committed
				
				// If the line is going to be visible, add it to the list of visible lines we will give to the container
				if (isLineVisible(textLine))
				{
					_curParcel.controller.addComposedLine(textLine);
					_paragraphContainsVisibleLines = true;
				}
				
				if (_parcelList.atEnd())
				{
					result = false;
					break;
				}
				
				_previousLine = textLine;
				
				// advance to the next element, using the rootElement of the container as a limitNode
				// to prevent going past the content bound to this container
				_curElementOffset += _curLine.textLength;
				if (_curElementOffset >= _curElement.textLength)
				{
					// We may have composed ahead over several spans; skip until we match up
					// Loop until we use catch up to where the line we just composed ended (pos).
					// Stop if we run out of elements. Skip empty inline elements, and skip floats
					// that came at the start of the line before any text -- they've already been 
					// processed.
					do{
						_curElementOffset -= _curElement.textLength;
						_curElementStart  += _curElement.textLength;
						_curElement = _curElement.getNextLeaf();
						if (_curElementStart == _curParaStart+_curParaElement.textLength)
							break;

						CONFIG::debug { assert(_curElement && _curElement.getParagraph() == _curParaElement,"composeParagraphElement: bad textLength in TextLine"); }
					} while (_curElementOffset >= _curElement.textLength || _curElement.textLength == 0 );
				}
				
				_paragraphSpaceCarried = _curLine.spaceAfter;
				
				// We finished composing the paragraph
				if (_curElementStart == _curParaStart + _curParaElement.textLength)
					break;
			}

			_parcelList.popLeftMargin(leftMargin);
			_parcelList.popRightMargin(rightMargin);
			if (firstLineIndent != 0)		// first line indent pop
			{
				if (_curParaFormat.direction == Direction.LTR)
					_parcelList.popLeftMargin(firstLineIndent);
				else
					_parcelList.popRightMargin(firstLineIndent);
				firstLineIndent = 0;
			}

			_previousLine = null;
			return result;
		}
		
		/** @private */
		protected function createTextLine(
			targetWidth:Number,	// target width we're composing into
			allowEmergencyBreaks:Boolean	// true to allow words to break in the middle of narrow columns, false to force overset
		):TextLine
		{
			var lineOffset:Number = (_curParaFormat.direction == Direction.LTR) ? _lineSlug.leftMargin : _lineSlug.rightMargin;     		
			
			var textLine:TextLine = null;
			textLine = TextLineRecycler.getLineForReuse();
			var textBlock:TextBlock = _curParaElement.getTextBlock();
			if (textLine)
			{
				CONFIG::debug { assert(_textFlow.backgroundManager == null || _textFlow.backgroundManager.getEntry(textLine) === undefined,"createTextLine - Bad TextLine in recycler cache"); }
				textLine = swfContext.callInContext(textBlock["recreateTextLine"],textBlock,[textLine, _previousLine, targetWidth, lineOffset, true]);
			}
			else
			{
				textLine = swfContext.callInContext(textBlock.createTextLine,textBlock,[_previousLine, targetWidth, lineOffset, true]);
			}
			CONFIG::debug { assert(!_previousLine || !textLine || _previousLine.textBlockBeginIndex + _previousLine.rawTextLength == textLine.textBlockBeginIndex, "FTE made non-contiguous TextLine"); }
			if (!allowEmergencyBreaks && textBlock.textLineCreationResult == TextLineCreationResult.EMERGENCY)
				textLine = null;

			// Unable to fit a new line
			if (textLine == null)
				return null;

			CONFIG::debug { assert(_curParaStart == _curParaElement.getAbsoluteStart(),"bad _curParaStart"); }

			_curLine.initialize(_curParaElement, targetWidth, lineOffset-_parcelList.insideListItemMargin, textLine.textBlockBeginIndex + _curParaStart, textLine.rawTextLength, textLine);
			CONFIG::debug { assert(_curLine.targetWidth == targetWidth,"Bad targetWidth"); }

			return textLine;
		}
		
		/** Called when we are about to compose a line. Handler for derived classes to override default behavior. */
		protected function startLine():void
		{
			_workingContentExtent = 0;
			_workingContentHeight = 0;
			_workingContentLogicalExtent = 0;
			_workingParcelIndex = _parcelList.currentParcelIndex;
			_workingTotalDepth = parcelList.totalDepth;
			_workingParcelLogicalTop = NaN;
		}
		
		protected function isLineVisible(textLine:TextLine):Boolean
		{ 
			// isLineVisible completes initializing textLine - so must call it even if we are _measuring
			return _curParcel.controller.isLineVisible(_blockProgression, _controllerVisibleBoundsXTW, _controllerVisibleBoundsYTW, _controllerVisibleBoundsWidthTW, _controllerVisibleBoundsHeightTW, _curLine, textLine) != null; 
		}
		
		/** Called when we are finished composing a line, and it is committed. Handler for derived classes to override default behavior.  */
		protected function endLine(textLine:TextLine):void	// No PMD
		{
			_contentCommittedExtent = Math.max(_contentCommittedExtent, _workingContentExtent);
			_contentCommittedHeight = Math.max(_contentCommittedHeight, _workingContentHeight);
			_contentLogicalExtent = Math.max(_contentLogicalExtent, _workingContentLogicalExtent);

			// if not measuring than contentLogicalExtent needs to match contentCommitedExtent so restarting composition in the middle gets the right extent
			// don't need contentLogicalExtent to exclude things pushing beyond the right margin as alignment is happening as we go
			if (!_measuring)
				_contentLogicalExtent = _contentCommittedExtent;
			if (_pushInFloats)
				_pushInFloats.length = 0;	// zero it out for the next line
			_atColumnStart = false;
			_linePass = 0;
			if (!isNaN(_workingParcelLogicalTop))
				_parcelLogicalTop = _workingParcelLogicalTop;
		}		
		
		protected function resetLine(textLine:TextLine):void
		{
			// if there is a BG manager remove the textLine since it isn't used
			if (_textFlow.backgroundManager)
				_textFlow.backgroundManager.removeLineFromCache(textLine);
			if (_workingParcelIndex != parcelList.currentParcelIndex)		// if we've moved to a new parcel, start over with the line
			{
				_linePass = 0;
				if (_pushInFloats)
					_pushInFloats.length = 0;
			}	
			else
				++_linePass;
			parcelList.addTotalDepth(_workingTotalDepth - _parcelList.totalDepth);
			_workingTotalDepth = parcelList.totalDepth;
		}
		
		protected function preProcessILGs(startPos:int):Boolean
		{
			if (!_curParcel)
				return false;
			
			var foundFloat:Boolean = false;
			
			// Before composing, run through the inline graphics and make sure the content elements are set up correctly
			// We don't support floats when we aren't wrapping the text
			var verticalText:Boolean = (_blockProgression == BlockProgression.RL);
			_forceILGs = (_parcelList.explicitLineBreaks || 
				(verticalText && _curParcel.controller.measureHeight) || (!verticalText && _curParcel.controller.measureWidth));

			for (var leaf:FlowLeafElement = _curParaElement.findLeaf(startPos); leaf; leaf = leaf.getNextLeaf(_curParaElement))
			{
				if (leaf is InlineGraphicElement)
				{
					var inlineGraphic:InlineGraphicElement = leaf as InlineGraphicElement;
					inlineGraphic.setEffectiveFloat(_forceILGs ? Float.NONE : inlineGraphic.computedFloat);
					foundFloat = true;
				}
			}
			return foundFloat;
		}
		
		// Called from composeParagraphElementIntoLines when we are starting to compose a line. Has hooks to handle floats.
		protected function processFloatsAtLineStart():void
		{
			if (_forceILGs)
				return;

			// There are two possible passes through this code for each line. On pass one, all floats at the
			// start of the line are processed, so they appear at the same level with the line (text in the 
			// line will wrap the float). On this pass, processFloatsAtLineEnd may find floats that are in the
			// middle of the line and "hoist" them by adding them to _pushInFloats. This will trigger a second
			// pass through the line, that will call back into this function. On the second pass, the floats
			// at the start have already been added to the parcel's knockout area, but we have to regenerate
			// their impact to the content bounds. Also, we have to compose the floats that were hoisted.
						
			// On the second pass, all floats that are hoisted will be in the array
			if (_pushInFloats && _pushInFloats.length > 0)
			{
				for (var i:int = 0; i < _pushInFloats.length; ++i)
				{
					var pos:int = _pushInFloats[i];
					var leaf:FlowLeafElement = _textFlow.findLeaf(pos);
					CONFIG::debug { assert(leaf is InlineGraphicElement, "pushed an element that is not a float"); }
					if (!composeFloat(leaf as InlineGraphicElement, false))	// If the float does not fit, cancel it and any following hoist requests
						_pushInFloats.length = i;
				}
			} 
		}
		
		// Return true if we tried to hoise some floats and need to retry the line
		protected function processFloatsAtLineEnd(textLine:TextLine):Boolean
		{
			// If there are no anchor points in the line, nothing to do
			if (!textLine.hasGraphicElement && _linePass <= 0)
				return true;
			
			// If the anchor point was pushed out, try again with one less float hoisted.
			if (_pushInFloats && _pushInFloats.length > 0)
			{
				var floatPosition:int = _pushInFloats[_pushInFloats.length - 1];
				if (_curLine.absoluteStart + _curLine.textLength <= floatPosition)
				{
					// Back out all the push in floats, remove the last push in float, and try again
					for (var floatIndex:int = _pushInFloats.length - 1; floatIndex >= 0; --floatIndex)
					{
						floatPosition = _pushInFloats[floatIndex];
						var elem:InlineGraphicElement = _textFlow.findLeaf(floatPosition) as InlineGraphicElement;
						var logicalFloatHeight:Number = (_blockProgression == BlockProgression.RL) ?
							elem.elementWidth + elem.getEffectivePaddingLeft() + elem.getEffectivePaddingRight() :
							elem.elementHeightWithMarginsAndPadding();
						var floatInfo:FloatCompositionData = _curLine.controller.getFloatAtPosition(floatPosition);
						if (floatInfo && floatInfo.absolutePosition == floatPosition)		// check: float might have been skipped if it's not loaded yet, in which case nothing to back out
						{
							var adjustTop:Number = isNaN(_lastLineDescent) ? 0 : _lastLineDescent;
							_curParcel.removeKnockOut(floatInfo.knockOutWidth, floatInfo.depth - adjustTop, floatInfo.depth + logicalFloatHeight, floatInfo.floatType == Float.LEFT);
						}
					}
					_curLine.controller.clearFloatsAt(_pushInFloats[0]);
					--_pushInFloats.length;
					return false;			// we've changed the floats, need to reset the line
				}
			}			
			
			var elementStart:int = _curElementStart;
			var element:FlowLeafElement = _curElement;
			var endPos:int = _curLine.absoluteStart + _curLine.textLength;
			var skipCount:int = 0;
						
			// Advance through the elements in the line, composing any InlineGraphicElements we find
			var hasInlines:Boolean = false;

			while (elementStart < endPos)
			{
				if (element is InlineGraphicElement)
				{
					var inlineGraphic:InlineGraphicElement = InlineGraphicElement(element);
					if (inlineGraphic.computedFloat == Float.NONE || _forceILGs)
						hasInlines = true;
					else 
					{
						if (_linePass == 0)
						{		// Hoist it. We will retry the line, composing this float at the start next time.
							if (!_pushInFloats)
								_pushInFloats = [];
							_pushInFloats.push(elementStart);
						}
						else if (_pushInFloats.indexOf(elementStart) >= 0)	// we hoisted, so skip it
							++skipCount;
						else 	// not hoisted
						{
							if (!composeFloat(inlineGraphic, true))	// Add it in below the line
							{
								advanceToNextParcel();
								return false;		// we need to start the line again in the next container; stop composing now 
							}
						}						
					}
				}
				elementStart += element.textLength;
				element = element.getNextLeaf();
			}
	
			var completed:Boolean = (skipCount >= (_pushInFloats ? _pushInFloats.length : 0));	// true if no floats need to be hoisted
			
			// process inline graphics if we have some, and we're on the last pass
			if (completed && hasInlines)
				processInlinesAtLineEnd(textLine);

			return completed;
		}
		
		// Process inline graphics for our current line.
		protected function processInlinesAtLineEnd(textLine:TextLine):void
		{
			// process inline graphics if we have some, and we're on the last pass
			var elementStart:int = _curElementStart;
			var element:FlowLeafElement = _curElement;
			var endPos:int = _curLine.absoluteStart + _curLine.textLength;
			while (elementStart < endPos)
			{
				if (element is InlineGraphicElement)
				{
					var inlineGraphic:InlineGraphicElement = element as InlineGraphicElement;
					if (inlineGraphic.computedFloat == Float.NONE || _forceILGs)
						composeInlineGraphicElement(inlineGraphic, textLine);
				}
				elementStart += element.textLength;
				element = element.getNextLeaf();
			}
		}
		
		protected function composeInlineGraphicElement(inlineGraphic:InlineGraphicElement, textLine:TextLine):Boolean
		{
			// Add the ILG to list of objects the container will update
			
			var marginAndPaddingX:Number = _blockProgression == BlockProgression.RL ? -inlineGraphic.getEffectivePaddingRight() : inlineGraphic.getEffectivePaddingLeft();
			var marginAndPaddingY:Number = inlineGraphic.getEffectivePaddingTop();

			// Get alpha and matrix values from FTE's inline placeholder
			var fteInline:DisplayObject = inlineGraphic.placeholderGraphic.parent;
			
			_curParcel.controller.addFloatAt(_curParaStart + inlineGraphic.getElementRelativeStart(_curParaElement), inlineGraphic.graphic, Float.NONE, 
				marginAndPaddingX, marginAndPaddingY, fteInline ? fteInline.alpha : 1, fteInline ? fteInline.transform.matrix : null, _parcelList.totalDepth, 0, 
				_curParcel.columnIndex, textLine);
			return true;
		}

		/*
		* Compose a floating graphic. Returns false if it doesn't fit in the parcel.
		*
		* @param elem	float we'e composing
		*/
		protected function composeFloat(elem:InlineGraphicElement, afterLine:Boolean):Boolean
		{
			if (elem.elementHeight == 0 || elem.elementWidth == 0)		// can't compose yet -- graphic isn't ready
				return true;
					
			if (_lastGoodStart == -1)
				_lastGoodStart = _curElementStart + _curElementOffset;
			
			var verticalText:Boolean = (_blockProgression == BlockProgression.RL);
			
			// HACK!!!
			//	if the baselineZero is set to ideographicTop, then the descent is the point size (measured from ideographic top)
			//	but in this case we've already factored that into the line height, so we're adding twice. Very confusing.
			var effectiveLastLineDescent:Number = 0;
			if ((afterLine ||  !_atColumnStart) && !isNaN(_lastLineDescent))
				effectiveLastLineDescent = _lastLineDescent;
			
			// If we're composing at the start of the paragraph, we may have spaceBefore (or spaceAfter from the previous paragraph)
			// that we need to take into account when placing the float.
			var spaceBefore:Number = 0;
			if (_curLine && _curParaElement != _curLine.paragraph && !_atColumnStart)
				spaceBefore = Math.max(_curParaElement.computedFormat.paragraphSpaceBefore, _paragraphSpaceCarried);
			var totalDepth:Number = _parcelList.totalDepth + spaceBefore + effectiveLastLineDescent;
			
			if (!_floatSlug)
				_floatSlug = new Slug();
			
			// See if it fits. If so, update the state to show the change. If not, remove the item.
			var logicalFloatWidth:Number;
			var logicalFloatHeight:Number;
			if (verticalText)
			{
				logicalFloatWidth = elem.elementHeight + elem.getEffectivePaddingTop() + elem.getEffectivePaddingBottom();
				logicalFloatHeight = elem.elementWidth + elem.getEffectivePaddingLeft() + elem.getEffectivePaddingRight();
			}
			else
			{
				logicalFloatWidth = elem.elementWidthWithMarginsAndPadding();
				logicalFloatHeight = elem.elementHeightWithMarginsAndPadding();
			}			
			
			var floatPosition:int = elem.getAbsoluteStart();
			var floatFits:Boolean = _parcelList.fitFloat(_floatSlug, totalDepth, logicalFloatWidth, logicalFloatHeight);
			
			// If the float is too wide but it fits in height, AND there is no text or other floats before it, allow it to overlap
			// the column so it doesn't go overset.
			if (!floatFits && (_curParcel.fitAny || _curParcel.fitsInHeight(totalDepth, int(logicalFloatHeight))) && (!_curLine || _curLine.absoluteStart == floatPosition || afterLine))
				floatFits = true;
			
			if (floatFits)
			{
				var floatType:String = elem.computedFloat;
				if (floatType == Float.START)
					floatType = (_curParaFormat.direction == Direction.LTR) ? Float.LEFT : Float.RIGHT;
				else if (floatType == Float.END)
					floatType = (_curParaFormat.direction == Direction.LTR) ? Float.RIGHT : Float.LEFT;
				
				var floatRect:Rectangle = calculateFloatBounds(elem, verticalText, floatType);
				
				// Take it into account for content bounds
				if (verticalText)
				{
					_workingContentExtent = Math.max(_workingContentExtent, floatRect.bottom);
					_workingContentHeight = Math.max(_workingContentHeight, _floatSlug.depth + floatRect.width);
					_workingContentLogicalExtent = Math.max(_workingContentLogicalExtent, floatRect.bottom);
					_accumulatedMinimumStart = Math.min(_accumulatedMinimumStart, floatRect.y);
				}
				else
				{
					_workingContentExtent = Math.max(_workingContentExtent, floatRect.right);
					_workingContentHeight = Math.max(_workingContentHeight, _floatSlug.depth + floatRect.height);
					_workingContentLogicalExtent = Math.max(_workingContentLogicalExtent, floatRect.right);
					_accumulatedMinimumStart = Math.min(_accumulatedMinimumStart, floatRect.x);
				}
				if (floatPosition == _curParcelStart)
					 _workingParcelLogicalTop = _floatSlug.depth;
				
				// floatRect is returned at new (x, y) location, sized to fit the column.
				var knockOutWidth:Number = ((floatType == Float.LEFT) ? _floatSlug.leftMargin : _floatSlug.rightMargin) + logicalFloatWidth;
				
				var adjustTop:Number = isNaN(_lastLineDescent) ? 0 : _lastLineDescent;
				_curParcel.knockOut(knockOutWidth, _floatSlug.depth - adjustTop, _floatSlug.depth + logicalFloatHeight, floatType == Float.LEFT);
				
				// Add info about the float so we can regenerate the knock out area if necessary for an incremental compose, as well as take care of update, etc.
				_curParcel.controller.addFloatAt(floatPosition, elem.graphic, floatType, floatRect.x, floatRect.y, elem.computedFormat.textAlpha, null, 
					_floatSlug.depth, knockOutWidth, _curParcel.columnIndex, _curParcel.controller.container);
				
				CONFIG::debug { _curParcel.controller.getFloatAt(_curParcel.controller.numFloats - 1).displacedVertically = totalDepth != _floatSlug.depth; } 
			}
		return floatFits;
		}
		
		private function calculateFloatBounds(elem:InlineGraphicElement, verticalText:Boolean, floatType:String):Rectangle
		{
			// We need to place it on the left or right side, sized to fit the graphic
			var floatRect:Rectangle = new Rectangle();
			if (verticalText)
			{
				floatRect.x = ((_curParcel.right - _floatSlug.depth) - elem.elementWidth) - elem.getEffectivePaddingRight();
				floatRect.y = (floatType == Float.LEFT) ? 
					(_curParcel.y + _floatSlug.leftMargin + elem.getEffectivePaddingTop()) : 
					(_curParcel.bottom - _floatSlug.rightMargin - elem.getEffectivePaddingBottom() - elem.elementHeight);
				floatRect.width = elem.elementWidth;
				floatRect.height = elem.elementHeight;
			}
			else
			{
				floatRect.x = (floatType == Float.LEFT) ? 
					_curParcel.x + _floatSlug.leftMargin + elem.getEffectivePaddingLeft() : 
					_curParcel.right - _floatSlug.rightMargin - elem.getEffectivePaddingRight() - elem.elementWidth;
				floatRect.y = _curParcel.y + _floatSlug.depth + elem.getEffectivePaddingTop();
				floatRect.width = elem.elementWidth;
				floatRect.height = elem.elementHeight;
			}
			return floatRect;
		}
		
		private function calculateLineWidthExplicit(textLine:TextLine):Number
			// Returns the content bounds width of a line set with explicit lineBreaks
		{
			var isRTL:Boolean = _curParaElement.computedFormat.direction == Direction.RTL;
			var lastAtom:int = textLine.atomCount - 1;
			// If we're at the end of the paragraph, don't count the terminator
			var endOfParagraph:Boolean = _curLine.absoluteStart + _curLine.textLength == _curParaStart + _curParaElement.textLength;
			if (endOfParagraph && !isRTL)
				--lastAtom;	// can go negative if just the terminator.  in that case use left/top of atom zero
			var bounds:Rectangle = textLine.getAtomBounds(lastAtom >= 0 ? lastAtom : 0);	// get rightmost atom bounds
			var lineWidth:Number = (_blockProgression == BlockProgression.TB) 
				? (lastAtom >= 0 ? bounds.right : bounds.left)
				: (lastAtom >= 0 ? bounds.bottom : bounds.top);
			if (isRTL)	// in right to left, get leftmost atom bounds, that has trailing space
			{
				// in RTL strip the width of the paragraph terminator from the front
				bounds = textLine.getAtomBounds(lastAtom != 0 && endOfParagraph ? 1 : 0);						
				lineWidth -= (_blockProgression == BlockProgression.TB) ? bounds.left : bounds.top;
			}
			textLine.flushAtomData();
			return lineWidth;
		}

		private function getRightSideGap(curLine:TextFlowLine, aligned:Boolean):Number
		{
			var elem:FlowGroupElement = curLine.paragraph;
			var paraFormat:ITextLayoutFormat = elem.computedFormat;
			
			var rightSideGap:Number = paraFormat.direction == Direction.RTL ? paraFormat.paragraphStartIndent : paraFormat.paragraphEndIndent;
			if (paraFormat.direction == Direction.RTL && (curLine.location & TextFlowLineLocation.FIRST))
			{
				// need to be careful because leftaligned paragraphs need to be exactly right coming out of this routine
				// if (aligned && (_blockProgression == BlockProgression.TB && !curLine.controller.measureWidth || _blockProgression == BlockProgression.RL && !curLine.controller.measureHeight))
				rightSideGap += paraFormat.textIndent;
				
				if (curLine.hasNumberLine && elem.getParentByType(ListItemElement).computedFormat.listStylePosition == ListStylePosition.INSIDE)
				{
					var textLine:TextLine = curLine.getTextLine(true);
					var numberLine:TextLine = TextFlowLine.findNumberLine(textLine)
					rightSideGap += TextFlowLine.getNumberLineInsideLineWidth(numberLine);
				}
			}

			do 
			{
				rightSideGap += _blockProgression == BlockProgression.TB ? elem.getEffectivePaddingRight() : elem.getEffectivePaddingBottom();
				elem = elem.parent;
			} while (!(elem is TextFlow))
			return rightSideGap;
		}
		
		private function getLeftSideGap(curLine:TextFlowLine):Number
		{
			var elem:FlowGroupElement = curLine.paragraph;
			var paraFormat:ITextLayoutFormat = elem.computedFormat;
			
			var leftSideGap:Number = paraFormat.direction == Direction.LTR ? paraFormat.paragraphStartIndent : paraFormat.paragraphEndIndent;
			
			if (paraFormat.direction == Direction.LTR && (curLine.location & TextFlowLineLocation.FIRST))
			{
				// recording leftSideIndent is here because there is an extra alignment step for non-left aligned paragraphs
				leftSideGap += paraFormat.textIndent;
				
				if (curLine.hasNumberLine && elem.getParentByType(ListItemElement).computedFormat.listStylePosition == ListStylePosition.INSIDE)
				{
					var textLine:TextLine = curLine.getTextLine(true);
					var numberLine:TextLine = TextFlowLine.findNumberLine(textLine)
					leftSideGap += TextFlowLine.getNumberLineInsideLineWidth(numberLine);
				}
			}
			
			do 
			{
				leftSideGap += _blockProgression == BlockProgression.TB ? elem.getEffectivePaddingLeft() : elem.getEffectivePaddingTop();
				elem = elem.parent;
			} while (!(elem is TextFlow))
			return leftSideGap;
		}
		
		/** @private */
		private function calculateLineAlignmentAndBounds(textLine:TextLine, numberLine:TextLine, alignData:AlignData):AlignData
		{
			var lineWidth:Number = _parcelList.explicitLineBreaks ? calculateLineWidthExplicit(textLine) : textLine.textWidth;
			
			var rightSideGap:Number = _lineSlug.rightMargin;
			var leftSideGap:Number = _lineSlug.leftMargin;
			var delta:Number = 0;
			
			CONFIG::debug 
			{ 
				if (textLine.userData != null &&_curParcel.controller.numFloats == 0)
				{
					var tmpRightGap:Number = getRightSideGap(_curLine, _curLine.alignment != TextAlign.LEFT);
					assert(rightSideGap == getRightSideGap(_curLine, _curLine.alignment != TextAlign.LEFT),"mismatched rightSideGap"); 
					
					var tmpLeftGap:Number = getLeftSideGap(_curLine);
					assert(leftSideGap  == getLeftSideGap(_curLine), "mismatched leftSideGap");
				}
			}
			
			CONFIG::debug { assert(textLine.userData == null || textLine.userData == _curLine,"Bad TextFlowLine in calculateLineAlignmentAndBounds"); }

			if (alignData)
			{
				alignData.rightSideGap = rightSideGap;
				alignData.leftSideGap = leftSideGap;
				alignData.lineWidth = lineWidth;
				alignData.textIndent = _curParaFormat.textIndent;
				
				var extraSpace:Number;
				var coord:Number;
				var adjustedLogicalRight:Number;
				
				
				if (_blockProgression == BlockProgression.TB)
				{		
					CONFIG::debug { assert(_curParcel.controller.measureWidth == _measuring,"Bad measuring initialization"); }
					if (!_measuring)
					{
						var textLineWidth:Number = textLine.textWidth;
						/*if (numberLine && TextFlowLine.getNumberLineListStylePosition(numberLine) == ListStylePosition.INSIDE)
							textLineWidth += TextFlowLine.getNumberLineInsideLineWidth(numberLine);*/
						extraSpace = _curParcel.width - leftSideGap - rightSideGap - textLineWidth;
						if (alignData.textAlign != TextAlign.LEFT)
						{
							delta = (alignData.textAlign == TextAlign.CENTER ? extraSpace / 2 : extraSpace);
							coord = _curParcel.x + leftSideGap + delta;
						}
						else	// used for RTL numberLine alignment of a left aligned textLine
							coord = _curParcel.x + leftSideGap + extraSpace;
						
						if (alignData.textAlign != TextAlign.LEFT)
						{
							_curLine.x = coord;
							textLine.x = coord;
						}
						else
							textLine.x = _curLine.x;
						
						if (numberLine && TextFlowLine.getNumberLineListStylePosition(numberLine) == ListStylePosition.OUTSIDE)
						{
							numberLine.x = computeNumberLineAlignment(alignData,textLine.textWidth,textLine.x,numberLine,coord,delta,extraSpace);
							CONFIG::debug { assert(alignData.textFlowLine == _curLine, "Mismatched alignData/curLine"); }
							_curLine.numberLinePosition = numberLine.x;
						}
						
						releaseAlignData(alignData);
						alignData = null;
					}
				}
				else 
				{
					CONFIG::debug { assert(_curParcel.controller.measureHeight == _measuring,"Bad measuring initialization"); }
					
					if (!_measuring)
					{						
						extraSpace = _curParcel.height - leftSideGap - rightSideGap -  textLine.textWidth;
						if (alignData.textAlign != TextAlign.LEFT)
						{
							delta = (alignData.textAlign == TextAlign.CENTER ? extraSpace / 2 : extraSpace);
							coord = _curParcel.y + leftSideGap + delta;
						}
						else	// used for RTL numberLine alignment of a left aligned textLine
							coord = _curParcel.y + leftSideGap + extraSpace;							
						
						if (alignData.textAlign != TextAlign.LEFT)
						{					
							_curLine.y = coord;
							textLine.y = coord;
						}
						else
							textLine.y = _curLine.y;
							
						if (numberLine && TextFlowLine.getNumberLineListStylePosition(numberLine) == ListStylePosition.OUTSIDE)
						{
							numberLine.y = computeNumberLineAlignment(alignData,textLine.textWidth,textLine.y,numberLine,coord,delta,extraSpace);
							CONFIG::debug { assert(alignData.textFlowLine == _curLine, "Mismatched alignData/curLine"); }
							_curLine.numberLinePosition = numberLine.y;
						}
	
						releaseAlignData(alignData);
						alignData = null;
					}	
				}
			}
			
			// extent from the left margin
			var lineExtent:Number = lineWidth + leftSideGap + rightSideGap + delta;
			// logical extent of the line
			_curLine.lineExtent = lineExtent;
			_workingContentLogicalExtent = Math.max(_workingContentLogicalExtent, lineExtent);
			_curLine.accumulatedLineExtent = Math.max(_contentLogicalExtent, _workingContentLogicalExtent);
			if (!alignData)
			{				
				// calculate this number - we're not measuring - otherwise its not really interesting
				var edgeAdjust:Number = _curParaFormat.direction == Direction.LTR ? Math.max(_curLine.lineOffset, 0) : _curParaFormat.paragraphEndIndent;
				edgeAdjust = _blockProgression == BlockProgression.RL ? _curLine.y - edgeAdjust : _curLine.x - edgeAdjust;
				if (numberLine)
				{
					var numberLineStart:Number = _blockProgression == BlockProgression.TB ? numberLine.x+_curLine.x : numberLine.y+_curLine.y;
					edgeAdjust = Math.min(edgeAdjust,numberLineStart);
					
					if (TextFlowLine.getNumberLineListStylePosition(numberLine) == ListStylePosition.OUTSIDE)
					{
						var numberLineMaxExtent:Number = numberLineStart + TextFlowLine.getNumberLineInsideLineWidth(numberLine);
						numberLineMaxExtent -= lineExtent; //  + 2 * (_blockProgression == BlockProgression.RL ? _curLine.y : _curLine.x);
						/*numberLineMaxExtent -= _blockProgression == BlockProgression.RL ? _curLine.y : _curLine.x;
						numberLineMaxExtent -= lineExtent-_curLine.x;*/
						if (numberLineMaxExtent > 0)
							delta += numberLineMaxExtent;
					}
				}
				_workingContentExtent = Math.max(_workingContentExtent, lineWidth + leftSideGap + Math.max(0,rightSideGap) + delta);

				_curLine.accumulatedMinimumStart = _accumulatedMinimumStart = Math.min(_accumulatedMinimumStart, edgeAdjust);
			}
	
			// first line
			if (_curLine.absoluteStart == _curParcelStart && isNaN(_workingParcelLogicalTop))
				_workingParcelLogicalTop = computeTextFlowLineMinimumLogicalTop(_curLine,textLine);

			return alignData;
		}
		
		/** @private - align the numberline so that its position is independent of the textAlign property */
		tlf_internal static function computeNumberLineAlignment(alignData:AlignData,textLineWidth:Number,textLineOffset:Number,numberLine:TextLine,coord:Number,delta:Number,extraSpace:Number):Number
		{
			var rslt:Number;
			
			if (alignData.textAlign == TextAlign.CENTER)
			{
				if (TextFlowLine.getNumberLineParagraphDirection(numberLine) == Direction.LTR)
					rslt = -(numberLine.textWidth + TextFlowLine.getListEndIndent(numberLine) + delta) - alignData.textIndent;
				else
					rslt = textLineWidth + TextFlowLine.getListEndIndent(numberLine) + (TextFlowLine.getNumberLineInsideLineWidth(numberLine)-numberLine.textWidth) + (coord - delta + extraSpace - textLineOffset) + alignData.textIndent;							
			}
			else if (alignData.textAlign == TextAlign.RIGHT)
			{
				if (TextFlowLine.getNumberLineParagraphDirection(numberLine) == Direction.LTR)
					rslt = -(numberLine.textWidth + TextFlowLine.getListEndIndent(numberLine) + delta) - alignData.textIndent;
				else
					rslt = textLineWidth + TextFlowLine.getListEndIndent(numberLine) + (TextFlowLine.getNumberLineInsideLineWidth(numberLine)-numberLine.textWidth) + alignData.textIndent;
			}
			else
			{
				if (TextFlowLine.getNumberLineParagraphDirection(numberLine) == Direction.LTR)
					rslt = -(numberLine.textWidth + TextFlowLine.getListEndIndent(numberLine)) - alignData.textIndent;
				else
					rslt = textLineWidth + TextFlowLine.getListEndIndent(numberLine) + (TextFlowLine.getNumberLineInsideLineWidth(numberLine)-numberLine.textWidth) + (coord - textLineOffset) + alignData.textIndent;
			}
			return rslt;
		}


		protected function composeNextLine():TextLine
		{
			CONFIG::debug { throw new Error("composeNextLine requires override"); }		
			return null;
		}
		
		// fills in _lineSlug
		protected function fitLineToParcel(textLine:TextLine, isNewLine:Boolean, numberLine:TextLine):Boolean
		{
			var composeYCoord:Number = _lineSlug.depth;
			_curLine.setController(_curParcel.controller,_curParcel.columnIndex);
			
			// If we are at the last parcel, we let text be clipped if that's specified in the configuration. At the point where no part of text can be accommodated, we go overset.
			// If we are not at the last parcel, we let text flow to the next parcel instead of getting clipped.
			var spaceBefore:Number = Math.max(_curLine.spaceBefore, _paragraphSpaceCarried);
			for (;;)
			{
				finishComposeLine(textLine, numberLine);	
				if (_parcelList.getLineSlug(_lineSlug, spaceBefore + (_parcelList.atLast() && _textFlow.configuration.overflowPolicy != OverflowPolicy.FIT_DESCENDERS ? _curLine.height-_curLine.ascent : _curLine.height+_curLine.descent), 1, _textIndent, _curParaFormat.direction == Direction.LTR))
				{
					// slug has moved, but the line is the same width; recalculate line position 
					// if the line width has changed, fitLineToParcel return false, and we regenerate the line
					if ((Twips.to(_lineSlug.width) == _curLine.outerTargetWidthTW) && (_lineSlug.depth != composeYCoord))
					{
						finishComposeLine(textLine, numberLine);	
					}
					break;
				}
				spaceBefore = _curLine.spaceBefore;
				if (_pushInFloats && _parcelList.currentParcel.fitAny && _pushInFloats.length > 0)		// force line to fit because the float fits, and we can scroll to the line
					break;
				
				for (;;)
				{
					advanceToNextParcel();
					if (!_curLine || _parcelList.atEnd())
						return false;
					if (_parcelList.getLineSlug(_lineSlug,0, 1, _textIndent, _curParaFormat.direction == Direction.LTR))
					{
						composeYCoord = _lineSlug.depth;
						break;
					}
				}
				_curLine.setController(_curParcel.controller,_curParcel.columnIndex);
			}
			
			// check to see if we got a good line
			if (Twips.to(_lineSlug.width) != _curLine.outerTargetWidthTW)
				return false;
			
			if(isNewLine)
			{
				if (numberLine)
					TextFlowLine.initializeNumberLinePosition(numberLine, _listItemElement, _curParaElement, textLine.textWidth);
				_curLine.createAdornments(_blockProgression,_curElement,_curElementStart, textLine, numberLine);
			}
			
			return true;
		}
		
		
		// Calculate paramters used in line height calculations for _curLine. The meaning of these parameters depends on the leading model (_curLineLeadingModel).
		// For LeadingModel.BOX, the first parameter is the bottom of the CSS line box, while the second parameter is the top of the CSS line box (both relative to Roman Baseline)
		// For all other leading models, the first parameter is the maximum computed lineHeight. The second parameter is ignored (0).
		// In all cases, _curLineLeading is updated to equal the first paramter, while the second parameter is returned.
		protected function calculateLeadingParameters (curElement:FlowLeafElement, curElementStart:int, numberLine:TextLine=null):Number
		{
			var effectiveListMarkerFormat:ITextLayoutFormat;
			if (numberLine)
				effectiveListMarkerFormat = TextFlowLine.getNumberLineSpanFormat(numberLine);
			
			if (_curLineLeadingModel == LeadingModel.BOX)
			{
				// Get the CSS "line box" for the current line. 
				var lineBox:Rectangle = _curLine.getCSSLineBox(_blockProgression, curElement, curElementStart, _textFlow.flowComposer.swfContext, effectiveListMarkerFormat, numberLine);
				_curLineLeading = lineBox ? lineBox.bottom : 0;
				return lineBox ? -lineBox.top : 0;	 
			}
			
			_curLineLeading = _curLine.getLineLeading(_blockProgression,curElement,curElementStart);
				
			// adjust for the NumberLine
			if (effectiveListMarkerFormat)
				_curLineLeading = Math.max(_curLineLeading,TextLayoutFormat.lineHeightProperty.computeActualPropertyValue(effectiveListMarkerFormat.lineHeight, effectiveListMarkerFormat.fontSize));
			
			return 0;
		}
	
		// Calculate the logical vertical position of the line, taking into account the leading (or firstBaselineOffset), 
		// and setting up the lineHeight and leading model info for the next line.
		protected function finishComposeLine(curTextLine:TextLine, numberLine:TextLine):void
		{      	
			var lineHeight:Number = 0;
			//replace X and Y with rise and run.  
			//	rise - the offset within a line relative to block progressiong.  For RL this is X, for TB Y
			//	run - the indentation of the line.  For RL this is Y, TB X
			var rise:Number;
			var run:Number;
			if (_blockProgression == BlockProgression.RL)
			{
				rise = (_curParcel.x + _curParcel.width) - _lineSlug.depth;
				run = _curParcel.y;
			}
			else
			{
				rise = _curParcel.y + _lineSlug.depth;
				run = _curParcel.x;
			}
			
		/*	if (_curParaFormat.direction == Direction.LTR)
				run += _curLine.lineOffset;
			else 
				run += _curLine.outerTargetWidth - _curLine.lineOffset - _curLine.targetWidth; */
			run += _lineSlug.leftMargin;
			
			_curLineLeadingModel = _curParaElement.getEffectiveLeadingModel();
			// Calculate leading parameters: _curLineLeading and secondaryLeadingParameter (the latter is currently only used for LeadingModel.BOX) 
			var secondaryLeadingParameter:Number = calculateLeadingParameters (_curElement, _curElementStart, numberLine);
			
			if (_curLineLeadingModel == LeadingModel.BOX)
			{
				lineHeight += _atColumnStart ? 0 : _lastLineDescent; // contribution from the previous line (if one exists): its "effective" descent
				lineHeight += secondaryLeadingParameter;			 // contribution from this line: the top of the CSS line box 	
			}
			else
			{
				var containerAttrs:ITextLayoutFormat = _curParcel.controller.computedFormat;		
				var baselineType:Object = BaselineOffset.LINE_HEIGHT;
				if (_atColumnStart)
				{
					// If we're at the top of the column, we need to check the container properties to see
					// what the firstBaselineOffset should be. This tells us how to treat the line.
					// However, when vertical alignment is center or bottom, ignore the firstBaselineOffset setting
					// and treat them as the BaselineOffset.AUTO case
					if (containerAttrs.firstBaselineOffset != BaselineOffset.AUTO && containerAttrs.verticalAlign != VerticalAlign.BOTTOM && containerAttrs.verticalAlign != VerticalAlign.MIDDLE) 
					{
						baselineType = containerAttrs.firstBaselineOffset;
						// The first line's offset is specified relative firstBaselineOffsetBasis, which used to be, but no longer is, a container-level property
						// Now it is implicitly deduced based on the container-level locale in the following manner: 
						// IDEOGRAPHIC_BOTTOM for ja and zh locales (this is the same locale set for which the default LeadingModel is IDEOGRAPHIC_TOP_DOWN)
						// ROMAN for all other locales
						var firstBaselineOffsetBasis:String = LocaleUtil.leadingModel(containerAttrs.locale) == LeadingModel.IDEOGRAPHIC_TOP_DOWN ?  TextBaseline.IDEOGRAPHIC_BOTTOM : TextBaseline.ROMAN;
						lineHeight -= curTextLine.getBaselinePosition(firstBaselineOffsetBasis);		
					}
					else
					{
						if (_curLineLeadingModel == LeadingModel.APPROXIMATE_TEXT_FIELD)
						{
							// Reinterpret AUTO when APPROXIMATE_TEXT_FIELD leading model is used. 
							// Align the "enhanced ascent" (an approximation of TextField's notion of ascent baseline, 
							// which differs from FTEs notion of the same by an amount equal to the line's descent) with the container top inset
							lineHeight += Math.round(curTextLine.descent) + Math.round(curTextLine.ascent)
							
							// Ensure Roman baseline will fall at an integer position. This is desirable for all leading models, 
							// but only APPROXIMATE_TEXT_FIELD requires it now. In a future release, this code can be moved below and lineX/lineY rounded off directly. 
							if (_blockProgression == BlockProgression.TB)
								lineHeight = Math.round(rise + lineHeight) - rise;
							else
								lineHeight = rise - Math.round(rise - lineHeight);
							
							baselineType = 0; // No further adjustments    
						}
						else
						{
							// The AUTO case requires aligning line top to container top inset. This efect can be achieved by using firstBaselineOffset=ASCENT
							// and firstBaselineOffsetBasis=ROMAN 
							baselineType = BaselineOffset.ASCENT;
							
							if(curTextLine.hasGraphicElement)
							{
								var firstLineAdjustment:LeadingAdjustment = getLineAdjustmentForInline(curTextLine, _curLineLeadingModel, true);
								if(firstLineAdjustment != null)
								{
									if(_blockProgression == BlockProgression.RL)
									{
										firstLineAdjustment.rise = -(firstLineAdjustment.rise);
									}
									_curLineLeading += firstLineAdjustment.leading;
									rise += firstLineAdjustment.rise;
								}
							}
							
							lineHeight -= curTextLine.getBaselinePosition(flash.text.engine.TextBaseline.ROMAN);
						}
					}
				}
						
				//getTextLineTypographicAscent
				if (baselineType == BaselineOffset.ASCENT)
				{
					CONFIG::debug { assert(_curElementStart == _textFlow.findLeaf(_curLine.absoluteStart).getAbsoluteStart(), "Bad _curElementStart"); }
					var curLineAscent:Number = _curLine.getLineTypographicAscent(_curElement,_curElementStart,curTextLine);
					if (numberLine)
					{
						// if numberlines support ilgs then additional information will be needed here to compute the typographic ascent
						CONFIG::debug { assert(!numberLine.hasGraphicElement, "numberline with graphic element unexpected"); }
						lineHeight += Math.max(curLineAscent,TextFlowLine.getTextLineTypographicAscent(numberLine,null,0,0));
					}
					else
						lineHeight += curLineAscent;
				}
				else 
				{
					if (baselineType == BaselineOffset.LINE_HEIGHT)
					{
						if (_curLineLeadingModel == LeadingModel.APPROXIMATE_TEXT_FIELD)
						{
							// Position the "enhanced ascent" (see above) at a distance of leading from the previous line's descent
							lineHeight += Math.round(_lastLineDescent) + Math.round(curTextLine.ascent) + Math.round(curTextLine.descent) + Math.round(_curLineLeading);
						}
						else if (_curLineLeadingModel == LeadingModel.ASCENT_DESCENT_UP)
						{
							lineHeight += _lastLineDescent + curTextLine.ascent + _curLineLeading;
						} 
						else
						{
							// Leading direction is irrelevant for the first line. Treat it as (UP, UP)
							// TODO-9/3/2008-It may be better to handle Middle/Last lines separately because we know that the previous line also belongs in the same para 
							var curLeadingDirectionUp:Boolean = _atColumnStart ? true : ParagraphElement.useUpLeadingDirection(_curLineLeadingModel);
							
							var prevLeadingDirectionUp:Boolean = _atColumnStart || _lastLineLeadingModel == "" ? true : 
								ParagraphElement.useUpLeadingDirection(_lastLineLeadingModel);
							
							var prevLineFirstElement:FlowLeafElement;
							
							if (curLeadingDirectionUp)
							{	
								//TODO-9/12/2008-The above behavior is the InDesign behavior but raises some questions about selection shapes.
								//Should selection code associate leading with the influencing line? That would be weird. InDesign only
								//supports alternate leading directions in the J feature set, where leading is never included in selection,
								//so this question does not arise. We take the unambiguous route: ignore leading DOWN at the end of a para
								lineHeight += _curLineLeading;
							}
							else
							{
								if (!prevLeadingDirectionUp)
								{
									// Same leading directions; use previous line's leading setting.
									lineHeight += _lastLineLeading;
								}
								else
								{
									// Make NO leading adjustments. Set lines solid.
									lineHeight += _lastLineDescent + curTextLine.ascent;
								}
							}	
						}
					}
					else
						lineHeight += Number(baselineType);		// fixed offset
				}
				
				//baselineType will be BaselineOffset.ASCENT for fixed leading
				if(curTextLine.hasGraphicElement && baselineType != BaselineOffset.ASCENT)
				{
					var adjustment:LeadingAdjustment = getLineAdjustmentForInline(curTextLine, _curLineLeadingModel, false);
					if(adjustment != null)
					{	
						if(_blockProgression == BlockProgression.RL)
						{
							adjustment.rise = -(adjustment.rise);
						}
						_curLineLeading += adjustment.leading;
						rise += adjustment.rise;
					}
				}
			}
			
			//don't know why, but ascent only needs to be removed from horizontal text.  Hmm, that seems
			//odd to me - gak 12.15.09
			//NOTE:9/27/10 - The ascent is added back for horizontal text in TextFlowLine.createShapeY
			rise += _blockProgression == BlockProgression.RL ? -(lineHeight) : lineHeight - curTextLine.ascent;
			
			// handle space before by adjusting y position of line
			// Space before does not apply to the first line, unless LeadingModel.BOX is used
			// Space carried never applies to the first line
			var spaceBefore:Number  = _atColumnStart && (_curLineLeadingModel != LeadingModel.BOX) ? 0 : _curLine.spaceBefore;
			var spaceCarried:Number = _atColumnStart ? 0 : _paragraphSpaceCarried;
			if (spaceBefore != 0 || spaceCarried != 0)
			{
				var spaceAdjust:Number = Math.max(spaceBefore, spaceCarried);	
				rise += _blockProgression == BlockProgression.RL ? -spaceAdjust :spaceAdjust;
			}
			
			if(_blockProgression == BlockProgression.TB)
				_curLine.setXYAndHeight(run,rise,lineHeight);
			else
				_curLine.setXYAndHeight(rise,run,lineHeight);
		}

		/** Delayed application of alignment when measuring */
		private function applyTextAlign(effectiveParcelWidth:Number):void
		{
			var textLine:TextLine;
			var numberLine:TextLine;
			var line:TextFlowLine;
			var alignData:AlignData;
			
			var coord:Number;
			var delta:Number;
			var adjustedLogicalRight:Number;
			var extraSpace:Number;
			var leftSideGap:Number;
			var rightSideGap:Number;
			var numberLineMetric:Number
			
			if (_blockProgression == BlockProgression.TB)
			{
				for each (alignData in _alignLines) 
				{
					textLine = alignData.textLine;
					
					rightSideGap = alignData.rightSideGap;
					leftSideGap = alignData.leftSideGap;
					
					extraSpace = effectiveParcelWidth - leftSideGap - rightSideGap -  textLine.textWidth;
					delta = (alignData.textAlign == TextAlign.CENTER ? extraSpace / 2 : extraSpace);
					coord = _curParcel.x + leftSideGap + delta;
					
					if (alignData.textAlign != TextAlign.LEFT)
					{
						line = textLine.userData as TextFlowLine;
						if (line)
							line.x = coord;
						textLine.x = coord;
					}

					adjustedLogicalRight = alignData.lineWidth + coord + Math.max(rightSideGap, 0);
					_parcelRight = Math.max(adjustedLogicalRight , _parcelRight);
					
					numberLine = TextFlowLine.findNumberLine(textLine);
					if (numberLine && TextFlowLine.getNumberLineListStylePosition(numberLine) == ListStylePosition.OUTSIDE)
					{
						numberLine.x = computeNumberLineAlignment(alignData,textLine.textWidth,textLine.x,numberLine,coord,delta,extraSpace);
						alignData.textFlowLine.numberLinePosition = numberLine.x;
						// adjust parcel bounds for the numberLine
						
						// the start
						numberLineMetric = numberLine.x+textLine.x;
						if (numberLineMetric < _parcelLeft)
							_parcelLeft = numberLineMetric;
						
						// the extent
						numberLineMetric += TextFlowLine.getNumberLineInsideLineWidth(numberLine);
						if (numberLineMetric > _parcelRight)
							_parcelRight = numberLineMetric;
					}
				}
			}
			else
			{
				for each (alignData in _alignLines) 
				{
					textLine = alignData.textLine;

					rightSideGap = alignData.rightSideGap;
					leftSideGap = alignData.leftSideGap;
					
					extraSpace = effectiveParcelWidth - leftSideGap - rightSideGap -  textLine.textWidth;
					delta = (alignData.textAlign == TextAlign.CENTER ? extraSpace / 2 : extraSpace);
					coord = _curParcel.y + leftSideGap + delta;
					
					if (alignData.textAlign != TextAlign.LEFT)
					{
						line = textLine.userData as TextFlowLine;
						if (line)
							line.y = coord;
						textLine.y = coord;
					}
					
					adjustedLogicalRight = alignData.lineWidth + coord + Math.max(rightSideGap, 0);
					_parcelBottom = Math.max(adjustedLogicalRight,_parcelBottom);
					
					numberLine = TextFlowLine.findNumberLine(textLine);
					if (numberLine && TextFlowLine.getNumberLineListStylePosition(numberLine) == ListStylePosition.OUTSIDE)
					{
						numberLine.y = computeNumberLineAlignment(alignData,textLine.textWidth,textLine.y,numberLine,coord,delta,extraSpace);
						alignData.textFlowLine.numberLinePosition = numberLine.y;	
						// adjust parcel bounds for the numberLine
						
						// the start
						numberLineMetric = numberLine.y+textLine.y;
						if (numberLineMetric < _parcelTop)
							_parcelTop = numberLineMetric;
						
						// the extent
						numberLineMetric += TextFlowLine.getNumberLineInsideLineWidth(numberLine);
						if (numberLineMetric > _parcelBottom)
							_parcelBottom = numberLineMetric;
					}
				}
			}
		}
		
		protected function commitLastLineState(curLine:TextFlowLine):void
		{
			// Remember leading-related state that may be used for laying out the next line
			_lastLineDescent = _curLineLeadingModel == LeadingModel.BOX ? _curLineLeading : curLine.descent;
			_lastLineLeading = _curLineLeading;
			_lastLineLeadingModel = _curLineLeadingModel;
		}
		
		protected function doVerticalAlignment(canVerticalAlign:Boolean,nextParcel:Parcel):void	// No PMD
		{
			// stub for required override
			CONFIG::debug { assert(false, "override in derived class"); }
		}
		
		protected function finalParcelAdjustment(controller:ContainerController):void	// No PMD
		{
			// stub for required override
			CONFIG::debug { assert(false, "finalParcelAdjustment missing override in derived class"); }
		}
		
		protected function finishParcel(controller:ContainerController,nextParcel:Parcel):Boolean
		{
			if (_curParcelStart == _curElementStart+_curElementOffset)		// empty parcel -- nothing composed into it
			{
				CONFIG::debug { assert(_contentLogicalExtent == 0,"bad contentlogicalextent on empty container"); }
				return false;
			}
			
			// Figure out the contents bounds information for the parcel we just finished composing
			
			// Content logical height is parcel depth, plus descenders of last line
			var totalDepth:Number = _parcelList.totalDepth;
			if (_textFlow.configuration.overflowPolicy == OverflowPolicy.FIT_DESCENDERS && !isNaN(_lastLineDescent))
				totalDepth += _lastLineDescent;
			totalDepth = Math.max(totalDepth, _contentCommittedHeight);
			
			// Initialize the parcel bounds
			// note we can later optimize away the adjustements
			if (_blockProgression == BlockProgression.TB)
			{
				_parcelLeft = _curParcel.x;
				_parcelTop = _curParcel.y;
				_parcelRight = _contentCommittedExtent+_curParcel.x;
				_parcelBottom = totalDepth+_curParcel.y;
			}
			else
			{
				// Push the values up to the controller running min/max, if they are bigger
				_parcelLeft = _curParcel.right-totalDepth;
				_parcelTop = _curParcel.y;
				_parcelRight = _curParcel.right;
				_parcelBottom = _contentCommittedExtent+_curParcel.y;
			}
			
			// delayed textalignment for horizontal lines
			if (_alignLines && _alignLines.length > 0)
			{
				CONFIG::debug 
				{ assert(_blockProgression == BlockProgression.TB && controller.measureWidth || controller.measureHeight,"finishParcel has lines to align and not measuring"); }
				applyTextAlign(_contentLogicalExtent);
				releaseAlignData(_alignLines[0]);
				_alignLines.length = 0;
			}
			
			// If we're measuring, then don't do vertical alignment
			var canVerticalAlign:Boolean = false;
			if (_blockProgression == BlockProgression.TB)
			{
				if (!controller.measureHeight && (!_curParcel.fitAny || _curElementStart + _curElementOffset >= _textFlow.textLength))
					canVerticalAlign = true;
			}
			else
			{
				if (!controller.measureWidth && (!_curParcel.fitAny || _curElementStart + _curElementOffset >= _textFlow.textLength))
					canVerticalAlign = true;
			}
			
			// need to always call this function because internal variables may need resetting
			doVerticalAlignment(canVerticalAlign,nextParcel);
			
			// This last adjustment is for two issues
			// 1) inline graphics that extend above the top (any ILGS I expect)
			// 2) negative first line indents (stil a worry here?)
			// If neither of these are present it can be skipped - TODO optimization
			// trace("BEF finalParcelAdjustment",_parcelLeft,_parcelRight,_parcelTop,_parcelBottom);
			finalParcelAdjustment(controller);
			// trace("AFT finalParcelAdjustment",_parcelLeft,_parcelRight,_parcelTop,_parcelBottom);
			_contentLogicalExtent = 0;
			_contentCommittedExtent = 0;
			_contentCommittedHeight = 0;
			_accumulatedMinimumStart = TextLine.MAX_LINE_WIDTH;
			
			return true;
		}
		
		/** apply vj and adjust the parcel bounds */
		protected function applyVerticalAlignmentToColumn(controller:ContainerController,vjType:String,lines:Array,beginIndex:int,numLines:int,beginFloatIndex:int,endFloatIndex:int):void
		{
			var firstLine:IVerticalJustificationLine = lines[beginIndex];
			var lastLine:IVerticalJustificationLine = lines[beginIndex+numLines-1]
			var firstLineCoord:Number;
			var lastLineCoord:Number;
			
			if (_blockProgression == BlockProgression.TB)
			{
				firstLineCoord = firstLine.y;
				lastLineCoord  = lastLine.y;
			}
			else
			{
				firstLineCoord = firstLine.x;
				lastLineCoord = lastLine.x;
			}
			
			var firstLineAdjustment:Number = VerticalJustifier.applyVerticalAlignmentToColumn(controller,vjType,lines,beginIndex,numLines, beginFloatIndex, endFloatIndex);
			
			if (!isNaN(_parcelLogicalTop))
				_parcelLogicalTop += firstLineAdjustment;
			
			if (_blockProgression == BlockProgression.TB)
			{
				_parcelTop += firstLine.y-firstLineCoord;
				_parcelBottom += lastLine.y-lastLineCoord;
			}
			else
			{
				_parcelRight += firstLine.x-firstLineCoord;
				_parcelLeft += lastLine.x-lastLineCoord;
			}
		}
		
		protected function finishController(controller:ContainerController):void
		{
			var controllerTextLength:int = _curElementStart + _curElementOffset - controller.absoluteStart;
			
			if (controllerTextLength != 0)
			{
				// Leave room for the padding. If the content overlaps the padding, don't count the padding twice.
				var paddingLeft:Number = controller.getTotalPaddingLeft();
				var paddingTop:Number = controller.getTotalPaddingTop();
				var paddingRight:Number = controller.getTotalPaddingRight();
				var paddingBottom:Number = controller.getTotalPaddingBottom();
				if (_blockProgression == BlockProgression.TB)
				{
					if (_controllerLeft > 0)
					{
						if (_controllerLeft < paddingLeft)
							_controllerLeft = 0;
						else 
							_controllerLeft -= paddingLeft;
					}
					
					if (_controllerTop > 0)
					{
						if (_controllerTop < paddingTop)
							_controllerTop = 0;
						else 
							_controllerTop -= paddingTop;
					}
					
					if (isNaN(controller.compositionWidth))
						_controllerRight += paddingRight;		 				
					else if (_controllerRight < controller.compositionWidth)
					{
						if (_controllerRight > controller.compositionWidth - paddingRight)
							_controllerRight = controller.compositionWidth;
						else 
							_controllerRight += paddingRight;
					}
					_controllerBottom += paddingBottom;	
				}
				else
				{
					_controllerLeft -= paddingLeft;
					if (_controllerTop > 0)
					{
						if (_controllerTop < paddingTop)
							_controllerTop = 0;
						else 
							_controllerTop -= paddingTop;
					}
					if (_controllerRight < 0)
					{
						if (_controllerRight > -paddingRight)
						{
							_controllerRight = 0;
						}
						else
							_controllerRight += paddingRight;
					}
					if (isNaN(controller.compositionHeight))
						_controllerBottom += paddingBottom;
					else if (_controllerBottom < controller.compositionHeight)
					{
						if (_controllerBottom > controller.compositionHeight - paddingBottom)
							_controllerBottom = controller.compositionHeight;
						else 
							_controllerBottom += paddingBottom;
					}
				}
				controller.setContentBounds(_controllerLeft, _controllerTop, _controllerRight-_controllerLeft, _controllerBottom-_controllerTop);
			}
			else
				controller.setContentBounds(0,0,0,0);
			
			controller.setTextLength(controllerTextLength);
			controller.finalParcelStart = _curParcelStart;
		}
		
		private function clearControllers(oldController:ContainerController, newController:ContainerController):void
		{
			// any controller between oldController and up to and including newController gets cleared
			var firstToClear:int = oldController ? _flowComposer.getControllerIndex(oldController)+1 : 0;
			var lastToClear:int  = newController ? _flowComposer.getControllerIndex(newController) : _flowComposer.numControllers-1;
			while (firstToClear <= lastToClear)
			{
				var controllerToClear:ContainerController = ContainerController(_flowComposer.getControllerAt(firstToClear));
				controllerToClear.setContentBounds(0, 0, 0, 0);
				controllerToClear.setTextLength(0);
				controllerToClear.clearComposedLines(controllerToClear.absoluteStart);
				controllerToClear.clearFloatsAt(controllerToClear.absoluteStart);
				firstToClear++;
			}
		}
		
		protected function advanceToNextParcel():void
		{
			parcelHasChanged(_parcelList.atLast() ? null : _parcelList.getParcelAt(_parcelList.currentParcelIndex + 1))

			_parcelList.next();
		}
		
		/** This is called when the parcel has changed 
		 * @param oldParcel - the parcel we had before (you can get the new parcel from the parcel list)
		 */
		protected function parcelHasChanged(newParcel:Parcel):void
		{
			var oldController:ContainerController = _curParcel ? ContainerController(_curParcel.controller) : null;
			var newController:ContainerController = newParcel  ? ContainerController(newParcel.controller)  : null;
			
			if (oldController != null && _lastGoodStart != -1)
			{
				oldController.clearFloatsAt(_lastGoodStart);
				_curLine = null;
				_linePass = 0;
				_pushInFloats.length = 0;
				// backup to lastGoodStart and try in next controller
			}
			
			/* if (newParcel)
			trace("parcelHasChanged newParcel: ",newParcel.clone().toString()); */
			
			if (_curParcel != null)
			{
				if (finishParcel(oldController,newParcel))
				{
					if (_parcelLeft < _controllerLeft)
						_controllerLeft = _parcelLeft;
					if (_parcelRight > _controllerRight)
						_controllerRight = _parcelRight;
					if (_parcelTop < _controllerTop)
						_controllerTop = _parcelTop;
					if (_parcelBottom > _controllerBottom)
						_controllerBottom = _parcelBottom;
				}
			}
			
			// update parcel data			
			if (oldController != newController)		// we're going on to the next controller in the chain
			{
				if (oldController)
					finishController(oldController);
				
				resetControllerBounds();
				
				if (_flowComposer.numControllers > 1)
				{
					if (oldController == null && _startController)
						clearControllers(_startController, newController);
					else
						clearControllers(oldController, newController);
				}
				if (newController)
				{
					CONFIG::debug { assert(!oldController || newController.absoluteStart == oldController.absoluteStart + oldController.textLength, "newController not yet set up"); }
					if (oldController)		// advance the start pos to the next controller if newController isn't the first controller
						_startComposePosition = newController.absoluteStart;
					calculateControllerVisibleBounds(newController);
				}
					
				// Parcel list will set totalDepth to newController's paddingTop
			}
			_curParcel = newParcel;
			_curParcelStart = _curElementStart+_curElementOffset;
			_atColumnStart = true;
			_workingTotalDepth = 0;
			if (newController)
			{
				_verticalSpaceCarried = (_blockProgression == BlockProgression.RL) ? newController.getTotalPaddingRight() : newController.getTotalPaddingTop();
				_measuring = _blockProgression == BlockProgression.TB && newController.measureWidth || _blockProgression == BlockProgression.RL && newController.measureHeight;
			}
		}
		
		/** Figure out which part of the controller is currently visible, based on the controller's current size and scroll position. 
		 * These values are used later to determine which lines are going to be visible.
		 */
		private function calculateControllerVisibleBounds(controller:ContainerController):void
		{
			// Similar computations also done in ContainerController.gatherVisibleLines
			var width:Number = controller.measureWidth ? Number.MAX_VALUE : controller.compositionWidth;
			var xScroll:Number = controller.horizontalScrollPosition;

			_controllerVisibleBoundsXTW = Twips.roundTo((_blockProgression == BlockProgression.RL) ? xScroll - width : xScroll);
			_controllerVisibleBoundsYTW = Twips.roundTo(controller.verticalScrollPosition);
			_controllerVisibleBoundsWidthTW = controller.measureWidth ? int.MAX_VALUE : Twips.to(controller.compositionWidth);
			_controllerVisibleBoundsHeightTW = controller.measureHeight ? int.MAX_VALUE : Twips.to(controller.compositionHeight); 
		}
		
		/** @private */
		private function getLineAdjustmentForInline(curTextLine:TextLine, curLeadingDir:String, isFirstLine:Boolean):LeadingAdjustment
		{
			var adjustment:LeadingAdjustment = null;
			var para:ParagraphElement = _curLine.paragraph;
			var flowElem:FlowLeafElement = _curElement; //the first element included in this line
			var curPos:int = flowElem.getAbsoluteStart();
			var largestPointSize:Number = flowElem.getEffectiveFontSize();
			var largestImg:Number = 0;
			
			//walk
			while(flowElem && curPos < _curLine.absoluteStart + _curLine.textLength)
			{
				if(curPos >= _curLine.absoluteStart || curPos + flowElem.textLength >= _curLine.absoluteStart)
				{	
					if(flowElem is InlineGraphicElement)
					{
						var inlineImg:InlineGraphicElement = flowElem as InlineGraphicElement;
						//we can ignore TCY for leading adjustments
						if (inlineImg.effectiveFloat == Float.NONE && !(_blockProgression == BlockProgression.RL && (flowElem.parent is TCYElement)))
						{
							//if the largest found img is smaller than the current image, we need new data
							if(largestImg < inlineImg.getEffectiveFontSize())
							{
								largestImg = inlineImg.getEffectiveFontSize();
								//only get this if the img is as large or larger than the largest found text
								if(largestImg >= largestPointSize)
								{
									largestImg = largestImg;
									var domBaseline:String = flowElem.computedFormat.dominantBaseline;
									if(domBaseline == FormatValue.AUTO)
										domBaseline = LocaleUtil.dominantBaseline(para.computedFormat.locale);
									
									//we are only making the adjustment for ideo-center, all others are to be ignored...
									if(domBaseline == TextBaseline.IDEOGRAPHIC_CENTER)
									{
										var curAdjustment:LeadingAdjustment = calculateLinePlacementAdjustment(curTextLine, domBaseline, curLeadingDir, inlineImg, isFirstLine);
										if(!adjustment || Math.abs(curAdjustment.rise) > Math.abs(adjustment.rise) || Math.abs(curAdjustment.leading) > Math.abs(adjustment.leading))
										{
											if(adjustment)
											{
												adjustment.rise = curAdjustment.rise;
												adjustment.leading = curAdjustment.leading;
											}
											else
												adjustment = curAdjustment;
										}
									}
								}
							}
						}
					}
					else
					{
						var tempSize:Number = flowElem.getEffectiveFontSize();
						if(largestPointSize <= tempSize)
						{
							largestPointSize = tempSize;
						}
						
						//if the largest image is smaller than this element, zero out the adjustment
						if(adjustment && largestImg < largestPointSize)
						{
							adjustment.leading = 0;
							adjustment.rise = 0;
						}
					}
				}
				
				//advance the position and get the next element
				curPos += flowElem.textLength;
				flowElem = flowElem.getNextLeaf(para);
			}
			return adjustment;
		}


		public function get swfContext():ISWFContext
		{ 
			var composerContext:ISWFContext = _flowComposer.swfContext;
			return composerContext ? composerContext : GlobalSWFContext.globalSWFContext; 
		}

		/** @private */
		private function calculateLinePlacementAdjustment(curTextLine:TextLine, domBaseline:String, curLeadingDir:String, inlineImg:InlineGraphicElement, isFirstLine:Boolean):LeadingAdjustment
		{
			var curAdjustment:LeadingAdjustment = new LeadingAdjustment();
			//get the leading height for the img
			var imgHeight:Number = inlineImg.getEffectiveLineHeight(_blockProgression);
			//get the leading as if the line contains no imgs.  We'll need this to adjust the total adjustments
			var lineLeading:Number = TextLayoutFormat.lineHeightProperty.computeActualPropertyValue(inlineImg.computedFormat.lineHeight, curTextLine.textHeight)
			
			//this is a redundant check, but will be needed in the future, so we're leaving it in. - gak 12.16.09
			if(domBaseline == TextBaseline.IDEOGRAPHIC_CENTER)
			{
				if(!isFirstLine)
				{
					//for non-first lines, we want to offset the rise of the line
					curAdjustment.rise += (imgHeight - lineLeading)/2;
				}
				else
				{
					//for the first line, the offset will be right, but hte leading wrong.
					curAdjustment.leading -= (imgHeight - lineLeading)/2;
				}
			}
			
			return curAdjustment;
		}
		
		protected function pushInsideListItemMargins(numberLine:TextLine):void
		{
			CONFIG::debug { assert(_listItemElement != null,"Bad call to pushInsideListItemMargins"); }
			if (numberLine && _listItemElement.computedFormat.listStylePosition == ListStylePosition.INSIDE)
			{
				var numberLineWidth:Number = TextFlowLine.getNumberLineInsideLineWidth(numberLine);
				_parcelList.pushInsideListItemMargin(numberLineWidth);
			}
		}
		
		protected function popInsideListItemMargins(numberLine:TextLine):void
		{
			CONFIG::debug { assert(numberLine == null || _listItemElement != null,"Bad call to pushInsideListItemMargins"); }
			if (numberLine && _listItemElement.computedFormat.listStylePosition == ListStylePosition.INSIDE)
			{
				var numberLineWidth:Number = TextFlowLine.getNumberLineInsideLineWidth(numberLine);
				_parcelList.popInsideListItemMargin(numberLineWidth);
			}
		}
	}
}

import flash.text.engine.TextLine;

import flashx.textLayout.compose.ISWFContext;
import flashx.textLayout.compose.TextFlowLine;
import flashx.textLayout.debug.Debugging;
import flashx.textLayout.debug.assert;
import flashx.textLayout.formats.TextAlign;
import flashx.textLayout.tlf_internal;

use namespace tlf_internal;

class AlignData 
{	public function AlignData(tfl:TextFlowLine)
	{ textFlowLine = tfl; }
	public var textFlowLine:TextFlowLine;
	public var textLine:TextLine;
	public var lineWidth:Number;
	public var textAlign:String;
	public var leftSideGap:Number;
	public var rightSideGap:Number;
	public var textIndent:Number;
}


class GlobalSWFContext implements ISWFContext
{
	static public const globalSWFContext:GlobalSWFContext = new GlobalSWFContext();

	public function GlobalSWFContext()
	{ }
	
	public function callInContext(fn:Function, thisArg:Object, argsArray:Array, returns:Boolean=true):*
	{
		CONFIG::debug
		{
			var rslt:*
			try
			{
				if (returns)
					rslt = fn.apply(thisArg, argsArray);

				else
					fn.apply(thisArg, argsArray);
					
				if (thisArg)
				{
					var traceArgs:Array;
					// later make this table driven
					if (thisArg.hasOwnProperty("createTextLine") && fn == thisArg["createTextLine"])
					{
						traceArgs = [rslt,thisArg,"createTextLine"]
						traceArgs.push.apply(traceArgs, argsArray);
						Debugging.traceFTECall.apply(null,traceArgs);
					}
					else if (thisArg.hasOwnProperty("recreateTextLine") && fn == thisArg["recreateTextLine"])
					{
						traceArgs = [rslt,thisArg,"recreateTextLine"]
						traceArgs.push.apply(traceArgs, argsArray);
						Debugging.traceFTECall.apply(null,traceArgs);
					}
				}
			}
			catch(e:Error)
			{
				// trace(e);
				throw(e);
			}
			return rslt;
		}
		CONFIG::release
		{
			if (returns)
				return fn.apply(thisArg, argsArray);
			fn.apply(thisArg, argsArray);
		}
	}
}

class LeadingAdjustment
{
	public var rise:Number = 0;
	public var leading:Number = 0;
	public var lineHeight:Number = 0;
}


