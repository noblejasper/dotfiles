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
	import flash.events.Event;
	import flash.events.EventDispatcher;
	import flash.events.IEventDispatcher;
	import flash.text.engine.TextLineValidity;
	import flash.utils.Dictionary;
	
	import flashx.textLayout.compose.FlowComposerBase;
	import flashx.textLayout.compose.IFlowComposer;
	import flashx.textLayout.compose.ISWFContext;
	import flashx.textLayout.compose.StandardFlowComposer;
	import flashx.textLayout.compose.TextFlowLine;
	import flashx.textLayout.container.ContainerController;
	import flashx.textLayout.debug.Debugging;
	import flashx.textLayout.debug.assert;
	import flashx.textLayout.edit.ISelectionManager;
	import flashx.textLayout.events.CompositionCompleteEvent;
	import flashx.textLayout.events.DamageEvent;
	import flashx.textLayout.events.ModelChange;
	import flashx.textLayout.events.StatusChangeEvent;
	import flashx.textLayout.formats.ITextLayoutFormat;
	import flashx.textLayout.formats.TextLayoutFormat;
	import flashx.textLayout.tlf_internal;
		
	use namespace tlf_internal;
	
	/**
	 *
	 *  @eventType flashx.textLayout.events.FlowOperationEvent.FLOW_OPERATION_BEGIN
	 *
	 * @playerversion Flash 10
	 * @playerversion AIR 1.5
	 * @langversion 3.0
	 */
	 
	[Event(name="flowOperationBegin", type="flashx.textLayout.events.FlowOperationEvent")]
	
	/**
	 * 
	 * @eventType flashx.textLayout.events.FlowOperationEvent.FLOW_OPERATION_END
	 *
	 * @playerversion Flash 10
	 * @playerversion AIR 1.5
	 * @langversion 3.0
	 */
	 
	[Event(name="flowOperationEnd", type="flashx.textLayout.events.FlowOperationEvent")]
	
	/**
	 * 
	 * @eventType flashx.textLayout.events.FlowOperationEvent.FLOW_OPERATION_COMPLETE
	 *
	 * @playerversion Flash 10
	 * @playerversion AIR 1.5
	 * @langversion 3.0
	 */
	
	[Event(name="flowOperationComplete", type="flashx.textLayout.events.FlowOperationEvent")]
	
	/** Dispatched whenever the selection is changed.  Primarily used to update selection-dependent user interface. 
	 * It can also be used to alter the selection, but cannot be used to alter the TextFlow itself.
	 *
	 * @playerversion Flash 10
	 * @playerversion AIR 1.5
	 * @langversion 3.0
	 */
	
	[Event(name="selectionChange", type="flashx.textLayout.events.SelectionEvent")]
	
	/** Dispatched after every recompose. 
	*
	* @playerversion Flash 10
	* @playerversion AIR 1.5
	* @langversion 3.0
	*/
	
	[Event(name="compositionComplete", type="flashx.textLayout.events.CompositionCompleteEvent")]
	
	/** Dispatched when the mouse is pressed down over any link. 
	 *
	 * @playerversion Flash 10
	 * @playerversion AIR 1.5
	 * @langversion 3.0
	 */
	
	[Event(name="mouseDown", type="flashx.textLayout.events.FlowElementMouseEvent")]
	
	/** Dispatched when the mouse is released over any link. 
	 *
	 * @playerversion Flash 10
	 * @playerversion AIR 1.5
	 * @langversion 3.0
	 */
	
	[Event(name="mouseUp", type="flashx.textLayout.events.FlowElementMouseEvent")]
	
	/** Dispatched when the mouse passes over any link. 
	 *
	 * @playerversion Flash 10
	 * @playerversion AIR 1.5
	 * @langversion 3.0
	 */
	
	[Event(name="mouseMove", type="flashx.textLayout.events.FlowElementMouseEvent")]	
	
	/** Dispatched when the mouse first enters any link. 
	 *
	 * @playerversion Flash 10
	 * @playerversion AIR 1.5
	 * @langversion 3.0
	 */
	
	[Event(name="rollOver", type="flashx.textLayout.events.FlowElementMouseEvent")]
	
	/** Dispatched when the mouse goes out of any link. 
	 *
	 * @playerversion Flash 10
	 * @playerversion AIR 1.5
	 * @langversion 3.0
	 */
	
	[Event(name="rollOut", type="flashx.textLayout.events.FlowElementMouseEvent")]	
	
	/** Dispatched when any link is clicked. 
	 *
	 * @playerversion Flash 10
	 * @playerversion AIR 1.5
	 * @langversion 3.0
	 */
	
	[Event(name="click", type="flashx.textLayout.events.FlowElementMouseEvent")]
	
	/** Dispatched when a InlineGraphicElement is resized due to having width or height as auto or percent 
	 * and the graphic has finished loading. 
	 *
	 * @playerversion Flash 10
	 * @playerversion AIR 1.5
	 * @langversion 3.0
	 */
	
	[Event(name="inlineGraphicStatusChanged", type="flashx.textLayout.events.StatusChangeEvent")]
	
	/** Dispatched by a TextFlow object after text is scrolled within a controller container.  
	 *
	 * @playerversion Flash 10
	 * @playerversion AIR 1.5
	 * @langversion 3.0
	 */
	
	[Event(name="scroll", type="flashx.textLayout.events.TextLayoutEvent")]
	
	/** Dispatched by a TextFlow object each time it is damaged 
	 * 
	 * You can use this event to find out that the TextFlow has changed, but do not access the TextFlow itself when this event 
	 * is sent out. This event is sent when TextFlow changes are partially complete, so it can be in an inconsistent state: 
	 * some changes have been mad already, and other changes are still pending. Get the information you need from the event, and make 
	 * required changes after control returns to your application.
	 *
	 * @playerversion Flash 10
	 * @playerversion AIR 1.5
	 * @langversion 3.0
	 */
	
	[Event(name="damage", type="flashx.textLayout.events.DamageEvent")]

	/** Dispatched by a TextFlow object each time a container has had new DisplayObjects added or updated as a result of composition.
	 *
	 * @playerversion Flash 10
	 * @playerversion AIR 1.5
	 * @langversion 3.0
	 */
	
	[Event(name="updateComplete", type="flashx.textLayout.events.UpdateCompleteEvent")]


	/**
	 * The TextFlow class is responsible for managing all the text content of a story. In TextLayout, text is stored in a 
	 * hierarchical tree of elements. TextFlow is the root object of the element tree. All elements on the tree
	 * derive from the base class, FlowElement. 
	 *
	 * <p>A TextFlow object can have ParagraphElement and DivElement objects as children. A div (DivElement object)
	 * represents a group of paragraphs (ParagraphElement objects). A paragraph can have SpanElement, InlineGraphicElement, 
	 * LinkElement, and TCYElement objects as children.</p>
	 * 
	 * <p>A span (SpanElement) is a range of text in a paragraph that has the same attributes. An image 
	 * (InlineGraphicElement) represents an arbitrary graphic that appears as a single character in a line of text. A 
	 * LinkElement represents a hyperlink, or HTML <code>a</code> tag, and it can contain multiple spans. A TCYElement object
	 * is used in Japanese text when there is a small run of text that appears perpendicular to the line, as in a horizontal
	 * run within a vertical line. A TCYElement also can contain multiple spans.</p>
	 
	 * <p>TextFlow also derives from the ContainerFormattedElement class, which is the root class for all container-level block 
	 * elements.</p>
 	 * <p>The following illustration shows the relationship of other elements, such as spans and paragraphs, to the TextFlow 
 	 * object.</p>
 	 * <p><img src="../../../images/textLayout_textFlowHierarchy.gif" alt="example TextFlow hierarchy"></img></p>
 	 *
 	 * <p>Each TextFlow object has a corresponding Configuration object that allows you to specify initial character and 
 	 * paragraph formats and the initial container format. It also allows you to specify attributes for selection, links, 
 	 * focus, and scrolling. When you supply a Configuration object as parameter to the <code>TextFlow()</code>
 	 * constructor, it creates a read-only snapshot that you can access through the <code>TextFlow.configuration</code>
 	 * property. After creation, you can't change the TextFlow's configuration. If you do not specify a Configuration, you 
 	 * can access the default configuration through the <code>TextFlow.defaultConfiguration</code> property.</p>
	 *
	 * @includeExample examples\TextFlowExample.as -noswf
	 *
	 * @playerversion Flash 10
	 * @playerversion AIR 1.5
	 * @langversion 3.0
	 *
	 * @see #configuration
	 * @see IConfiguration
	 * @see DivElement
	 * @see FlowElement
	 * @see FlowGroupElement
	 * @see FlowLeafElement
	 * @see flashx.textLayout.compose.IFlowComposer IFlowComposer
	 * @see ParagraphElement
	 * @see SpanElement
	 */
	 
	public class TextFlow extends ContainerFormattedElement implements IEventDispatcher
	{		
		private var _flowComposer:IFlowComposer;
		
		/** References the Selection manager attached to this TextFlow object. */
		private var _interactionManager:ISelectionManager;
		
		/** References the Configuration object for this TextFlow object. */
		
		private var _configuration:IConfiguration;
		
		/** Manages computing and drawing backgroundColor attribute */
		private var _backgroundManager:BackgroundManager;
		
		/** Default configuration for all new TextFlow objects if the configuration is not specified. 
		 *
		 * @playerversion Flash 10
		 * @playerversion AIR 1.5
	 	 * @langversion 3.0
	 	 *
	 	 * @see Configuration
		 */
		
		public static var defaultConfiguration:Configuration = new Configuration();
				
		// normalize support
		private var normalizeStart:int = 0;
		private var normalizeLen:int = 0;
		
		// event dispatch support; lazy initialization
		private var _eventDispatcher:EventDispatcher;
		
		// textflow specific generation support - used to validate undo
		private var _generation:uint;
		// next generation number to hand out - these just have to be unique so share one.
		// 0 is reserved to mean "not set"
		static private var _nextGeneration:uint = 1;
		
		// styling support
		private var _formatResolver:IFormatResolver;
		
		// interactive object count - now LinkElements later add in elements with eventMirrors
		private var _interactiveObjectCount:int;
		
		// ILG count
		private var _graphicObjectCount:int;
				
		/** 
		 * Constructor - creates a new TextFlow instance.
		 *
		 * <p>If you provide a <code>config</code> parameter, the contents of the Configuration object are copied and
		 * you cannot make changes. You can access configuration settings, however, through the 
		 * <code>configuration</code> property. If the <code>config</code> parameter is null, you can access the default
		 * configuration settings through the <code>defaultConfiguration</code> property.</p> 
		 *
		 * <p>The Configuration object provides a mechanism for setting configurable default attributes on a TextFlow.  
		 * While you can't make changes to the Configuration object, you can override default attributes, if necessary, 
		 * by setting the attributes of TextFlow and its children.</p>
		 * 
		 * @param config Specifies the configuration to use for this TextFlow object. If it's null, use 
		 * <code>TextFlow.defaultConfiguration</code> to access configuration values. 
		 *
		 * @playerversion Flash 10
		 * @playerversion AIR 1.5
	 	 * @langversion 3.0
	 	 * 
	 	 * @see Configuration
	 	 * @see #configuration
	 	 * @see #defaultConfiguration
		 * 
		 */
		 
		public function TextFlow(config:IConfiguration = null)
		{
			super();
			initializeForConstructor(config);
		}
		
		private function initializeForConstructor(config:IConfiguration):void
		{
			if (config == null)
				config = defaultConfiguration;
			// read only non changing copy of current state
			_configuration = Configuration(config).getImmutableClone();
			// do not set up the event dispatcher yet
			
			format = _configuration.textFlowInitialFormat;
			
			// initialize the flowComposer
			if (_configuration.flowComposerClass)
				flowComposer = new _configuration.flowComposerClass();
			
			_generation = _nextGeneration++;
			_interactiveObjectCount = 0;
			_graphicObjectCount = 0;
		}
		
		/** @private */
		public override function shallowCopy(startPos:int = 0, endPos:int = -1):FlowElement
		{		
			var retFlow:TextFlow = super.shallowCopy(startPos, endPos) as TextFlow;
			retFlow._configuration = _configuration;
			retFlow._generation = _nextGeneration++;
			if (formatResolver)
				retFlow.formatResolver = formatResolver.getResolverForNewFlow(this,retFlow);
			// TODO: preserve the hostFormat??
			// preserve the swfContext
			if (retFlow.flowComposer && flowComposer)
				retFlow.flowComposer.swfContext = flowComposer.swfContext;
			return retFlow;						
		}
		
		/** @private - count of interactive objects */
		tlf_internal function get interactiveObjectCount():int
		{ return _interactiveObjectCount; }
		
		/** @private - increment the count */
		tlf_internal function incInteractiveObjectCount():void
		{ _interactiveObjectCount++; } 
		
		/** @private - decrement the count */
		tlf_internal function decInteractiveObjectCount():void
		{ _interactiveObjectCount--; }
		
		/** @private - count of interactive objects */
		tlf_internal function get graphicObjectCount():int
		{ return _graphicObjectCount; }
		
		/** @private - increment the count */
		tlf_internal function incGraphicObjectCount():void
		{ _graphicObjectCount++; } 
		
		/** @private - decrement the count */
		tlf_internal function decGraphicObjectCount():void
		{ _graphicObjectCount--; }
		
		/** 
		* The Configuration object for this TextFlow object. The Configuration object specifies the initial character 
		* and paragraph formats, the initial container format, and attributes for selection highlighting, 
		* links, focus, and scrolling.
		*
		* <p>If you do not specify a Configuration object, Text Layout Framework uses a default Configuration object, which
		* is referenced by the <code>defaultConfiguration</code> property.</p>
		*
		* @playerversion Flash 10
		* @playerversion AIR 1.5
	 	* @langversion 3.0
	 	*
	 	* @see Configuration
	 	* @see #defaultConfiguration
	 	*/
		public function get configuration():IConfiguration
		{ return _configuration; }

		/**
		 * The InteractionManager associated with this TextFlow object.
		 * <p>Controls all selection and editing on the text. If the TextFlow is not selectable, 
		 * the interactionManager is null. To make the TextFlow editable, assign a interactionManager
		 * that is both an ISelectionManager and an IEditManager. To make a TextFlow that is read-only
		 * and allows selection, assign a interactionManager that is an ISelectionManager only. </p>
		 *
		 * @playerversion Flash 10
		 * @playerversion AIR 1.5
	 	 * @langversion 3.0
	 	 *
		 * @see flashx.textLayout.edit.ISelectionManager ISelectionManager
		 * @see flashx.textLayout.edit.IEditManager IEditManager
		 */
		public function get interactionManager():ISelectionManager
		{
			return _interactionManager;
		}
		public function set interactionManager(newInteractionManager:ISelectionManager):void
		{
			// detatch old interactionManager
			if (_interactionManager != newInteractionManager)
			{
				if (_interactionManager)
					_interactionManager.textFlow = null;
				_interactionManager = newInteractionManager;
				if (_interactionManager)
				{
					_interactionManager.textFlow = this;
					normalize();
				}
				if (flowComposer)
					flowComposer.interactionManagerChanged(newInteractionManager);
			}
		}
		
		/** Manages the containers for this element.
		 * 
		 * <p>The TextLines that are created from the element appear as children of the container.
		 * The flowComposer manages the containers, and as the text is edited it adds lines to and removes lines
		 * from the containers. The flowComposer also keeps track of some critical attributes, such as the
		 * width and height to compose to, whether scrolling is on, and so on.</p>
		 * 
		 * <p>The container and <code>flowComposer</code> are closely related. If you reset <code>flowComposer</code>, 
		 * the container is reset to the new flowComposer's container. Likewise if the container is reset, 
		 * <code>flowComposer</code> is reset to the container's new flowComposer.</p>
		 *
		 * @playerversion Flash 10
		 * @playerversion AIR 1.5
	 	 * @langversion 3.0
	 	 *
	 	 * @see flashx.textLayout.compose.IFlowComposer FlowComposer
		 */
		public override function get flowComposer():IFlowComposer
		{ 
			return _flowComposer; 
		}
		
		public function set flowComposer(composer:IFlowComposer):void
		{
			changeFlowComposer(composer,true);
		}
		
		/** @private use this function directly if you want to clear the flowcomposer but not unload the graphics.  */
		tlf_internal function changeFlowComposer(newComposer:IFlowComposer,okToUnloadGraphics:Boolean):void
		{
			var origComposer:IFlowComposer = _flowComposer;
			if (_flowComposer != newComposer)
			{
				var oldSWFContext:ISWFContext = FlowComposerBase.computeBaseSWFContext(_flowComposer ? _flowComposer.swfContext : null);
				var newSWFContext:ISWFContext = FlowComposerBase.computeBaseSWFContext(newComposer ? newComposer.swfContext : null);					
				
				// Clear out old settings
				if (_flowComposer)
				{
					//hideSelection is no longer on IFlowComposer, so do it manually
					var containerIter:int = 0;
					while(containerIter < _flowComposer.numControllers)
						_flowComposer.getControllerAt(containerIter++).clearSelectionShapes();
					
					_flowComposer.setRootElement(null); 	// clear event listeners
				}

				_flowComposer = newComposer;
	
				if (_flowComposer)
					_flowComposer.setRootElement(this);
					
				// Mark flow as damaged
				if (textLength)
					damage(getAbsoluteStart(), textLength, TextLineValidity.INVALID, false);
				
				if (oldSWFContext != newSWFContext)
					invalidateAllFormats();
	
				// containers *may* have changed requiring reinherit of columnDirection/blockProgression
				// but that's only in the case when we have ContainerFormattedElements that can have containers -- if then
				// this call is really expensive for long flows and needs to be optimized for cases when nothing changes
				// containerFormatChanged(false);

				// no longer visible shut down all the InlineGraphicElements
				if (_flowComposer == null)
				{
					if (okToUnloadGraphics)
						unloadGraphics();
				}
				else if (origComposer == null)
					prepareGraphicsForLoad();
			}
		}
		
		/** @private - use to unload ILGs.  Generally TLF manage this for you but TLF errs on the side of letting the graphics run once they are started.  There may
		 * be cases where the client will want to unload the graphics. */
		tlf_internal function unloadGraphics():void
		{
			if (_graphicObjectCount)
				applyFunctionToElements(function (elem:FlowElement):Boolean{ if (elem is InlineGraphicElement) (elem as InlineGraphicElement).stop(true); return false; });			
		}
		
		/** @private - use to queue ILGs for loading.  Generally TLF manage this for you.  However, this function exists so that clients may initiate a load in edge cases. 
		 * Graphics aren't loaded until the next updateAllControllers call.  */
		tlf_internal function prepareGraphicsForLoad():void
		{
			// queue for start/restart any graphics that are not loaded but have a URL or CLASS source
			if (_graphicObjectCount)
				appendElementsForDelayedUpdate(this,null);			
		}
		
		/** Returns an element whose <code>id</code> property matches the <code>idName</code> parameter. Provides
		 * the ability to apply a style based on the <code>id</code>. 
		 *
		 * <p>For example, the following line sets the style "color" to 0xFF0000 (red), for the
		 * element having the <code>id</code> span1.</p>
		 *
		 * <listing version="3.0" >
		 * textFlow.getElementByID("span1").setStyle("color", 0xFF0000);
		 * </listing>
		 *
		 * <p><strong>Note:</strong> In the following code, <code>p.addChild(s)</code> <em>removes</em> <code>s</code> 
		 * from its original parent and adds it to <code>p</code>, the new parent.</p>
		 *
		 * <listing version="3.0" >
		 * var s:SpanElement = new SpanElement();
		 * var p:ParagraphElement = new ParagraphElement();
		 * ...
		 * s = textFlow.getElementByID("span3") as SpanElement;
		 * p.addChild(s);
		 * textFlow.addChild(p);
		 * </listing>
		 *
		 * @param idName The <code>id</code> value of the element to find.
		 *
		 * @return The element whose id matches <code>idName</code>.
		 *
		 * @includeExample examples\TextFlow_getElementByIDExample.as -noswf
		 *
		 * @playerversion Flash 10
		 * @playerversion AIR 1.5
	 	 * @langversion 3.0
		 *
		 * @see FlowElement#id 
		 */
		
		public function getElementByID(idName:String):FlowElement
		{
			var rslt:FlowElement;
			applyFunctionToElements(function (elem:FlowElement):Boolean{ if (elem.id == idName) { rslt = elem; return true; } return false; });
			return rslt;
		}

		/** Returns all elements that have <code>styleName</code> set to <code>styleNameValue</code>.
		 *
		 * @param styleNameValue The name of the style for which to find elements that have it set.
		 *
		 * @return An array of the elements whose <code>styleName</code> value matches <code>styleNameValue</code>. For example,
		 * all elements that have the style name "color".
		 *
		 * @playerversion Flash 10
		 * @playerversion AIR 1.5
	 	 * @langversion 3.0
		 *
		 * @see FlowElement#styleName 
		 */
		public function getElementsByStyleName(styleNameValue:String):Array
		{
			var a:Array = new Array;
			applyFunctionToElements(function (elem:FlowElement):Boolean{ if (elem.styleName == styleNameValue) a.push(elem); return false; });
			return a;
		}
		
		/** Returns all elements that have <code>typeName</code> set to <code>typeNameValue</code>.
		 *
		 * @param styleNameValue The name of the style for which to find elements that have it set.
		 *
		 * @return An array of the elements whose <code>typeName</code> value matches <code>typeNameValue</code>. For example,
		 * all elements that have the type name "foo".
		 *
		 * @playerversion Flash 10
		 * @playerversion AIR 1.5
		 * @langversion 3.0
		 *
		 * @see FlowElement#styleName 
		 */
		public function getElementsByTypeName(typeNameValue:String):Array
		{
			var a:Array = new Array;
			applyFunctionToElements(function (elem:FlowElement):Boolean{ if (elem.typeName == typeNameValue) a.push(elem); return false; });
			return a;
		}

		/** @private */
		override protected function get abstract():Boolean
		{ return false; }
		
		/** @private */
		tlf_internal override function get defaultTypeName():String
		{ return "TextFlow"; }
		
		/** @private */
		tlf_internal override function updateLengths(startIdx:int,len:int,updateLines:Boolean):void
		{
			if (normalizeStart != -1)
			{
				var newNormalizeStart:int = startIdx < normalizeStart ? startIdx : normalizeStart;

				if (newNormalizeStart < normalizeStart)
					normalizeLen += (normalizeStart-newNormalizeStart);
				normalizeLen += len;
				normalizeStart = newNormalizeStart;
			}
			else
			{
				normalizeStart = startIdx;
				normalizeLen   = len;
			}
			// never go below zero
			if (normalizeLen < 0)
				normalizeLen = 0;
			
			// fix the lines
			if (updateLines && _flowComposer)
			{
				_flowComposer.updateLengths(startIdx,len);
				super.updateLengths(startIdx,len, false);
			}
			else
				super.updateLengths(startIdx,len,updateLines);
		}
		
		[RichTextContent]
		/** @private NOTE: all FlowElement implementers and overrides of mxmlChildren must specify [RichTextContent] metadata */
		public override function set mxmlChildren(array:Array):void
		{
			super.mxmlChildren = array; 
			normalize();
			applyWhiteSpaceCollapse(null);
		} 
		
		/** @private Update any elements that have a delayed updated.  Normally used to stop foreignelements when they 
		 * are either displayed the first time or removed from the stage
		 */
		tlf_internal function applyUpdateElements(okToUnloadGraphics:Boolean):Boolean
		{
			if (_elemsToUpdate)
			{
				var hasController:Boolean = flowComposer && flowComposer.numControllers != 0;
				for (var child:Object in _elemsToUpdate)
					(child as FlowElement).applyDelayedElementUpdate(this,okToUnloadGraphics,hasController);
				// if there is a controller then done with this list.  
				// if no controller have to keep the list around because they may be in the list waiting for load on a future compose 
				// the scenario that preserving the list fixes is a compose with no controllers followed by a compose with controllers
				if (hasController)
				{
					_elemsToUpdate = null;
					return true;
				}
			}
			return false;
		}
		
		/** @private */
		tlf_internal override function preCompose():void
		{
			// normalizes the flow
			do 
			{
				normalize();
			}	
				// starts or stops any FEs that have been modified, removed or deleted
			while (applyUpdateElements(true));
			
			// need to call normalize again in case any of the element updates have modified the hierarchy
			// normalize();
		}
	
		/**
		 * Mark the a range of text as invalid - needs to be recomposed.
		 * <p>The text classes are self damaging.  This is only used when modifying the container chain.</p>
		 * <p>Warning: Plan to evaulate a way to hide this method totally.</p>
		 * @param start		text index of first character to marked invalid
		 * @param damageLen	number of characters to mark invalid
		 * @param needNormalize optional parameter (true is default) - normalize should include this range.
		 * @private
		 */
		tlf_internal function damage(damageStart:int, damageLen:int, damageType:String, needNormalize:Boolean = true):void
		{
			// CONFIG::debug { assert(damageLen > 0,"must have at least 1 char in damageLen"); }
				
			if (needNormalize)
			{
				if (normalizeStart == -1)
				{
					normalizeStart = damageStart;
					normalizeLen   = damageLen;
				}
				else
				{
					if (damageStart < normalizeStart)
					{
						var newNormalizeLen:uint = normalizeLen;
						newNormalizeLen = normalizeStart+normalizeLen - damageStart;
						if (damageLen > newNormalizeLen)
							newNormalizeLen = damageLen;
						normalizeStart = damageStart;
						normalizeLen = newNormalizeLen;
					}
					else if ((normalizeStart+normalizeLen) > damageStart)
					{
						if (damageStart+damageLen > normalizeStart+normalizeLen)
							normalizeLen = damageStart+damageLen-normalizeStart;
					}
					else
						normalizeLen = damageStart+damageLen-normalizeStart;
				}
				
				// clamp to textLength
				CONFIG::debug { assert(normalizeStart <= textLength,"damage bad length"); }
				if (normalizeStart+normalizeLen > textLength)
					normalizeLen = textLength-normalizeStart;
				// trace("damage damageStart:" + damageStart.toString() + " damageLen:" + damageLen.toString() + " textLength:" + this.textLength + " normalizeStart:" + normalizeStart.toString() + " normalizeLen:"  + normalizeLen.toString() );
			}
			
			if (_flowComposer)
				_flowComposer.damage(damageStart, damageLen, damageType);
				
			if (hasEventListener(DamageEvent.DAMAGE))
				dispatchEvent(new DamageEvent(DamageEvent.DAMAGE,false,false,this,damageStart,damageLen));
		}
			
		/**
		 * Find the paragraph at the specified absolute position
		 * @private
		 */
		tlf_internal function findAbsoluteParagraph(pos:int):ParagraphElement
		{
			var elem:FlowElement = findLeaf(pos);
			return elem ? elem.getParagraph() : null;
		}

		/**
		 * Find the FlowGroupElement at the absolute position,
		 * could be synonymous with the paragraph OR a subBlockElement
		 * @private
		 */
		 tlf_internal function findAbsoluteFlowGroupElement(pos:int):FlowGroupElement
		 {
		 	var elem:FlowElement = findLeaf(pos);
		 	return elem.parent;
		 }
		
		/** @private */
		CONFIG::debug public override function debugCheckFlowElement(depth:int = 0, extraData:String = ""):int
		{
			// debugging function that asserts if the flow element tree is in an invalid state
			
			var rslt:int = super.debugCheckFlowElement(depth,extraData);
			
			// describe the lines
			if (Debugging.verbose && flowComposer)
			{
				for ( var lineIdx:int = 0; lineIdx < flowComposer.numLines; lineIdx++)
				{
					var workLine:TextFlowLine = flowComposer.getLineAt(lineIdx);
					var containerIdx:int = flowComposer.getControllerIndex(workLine.controller);
					trace("line:",lineIdx,"controller:",containerIdx,workLine.toString());
				}
				
				for (var idx:int = 0; idx < flowComposer.numControllers; idx++)
				{
					var controller:ContainerController = flowComposer.getControllerAt(idx);
					trace("controller:",idx,Debugging.getIdentity(controller),controller.absoluteStart,controller.textLength,controller.compositionWidth,controller.compositionHeight,controller.getContentBounds());
				}
			}
			
			rslt += assert(parent == null, "TextFlow should not have a parent");
			rslt += assert(parentRelativeStart == 0, "TextFlow start not zero");

			return rslt;		
		}
		
		/**
		 * Check the internal consistency of the flow's FlowElement tree.
		 * @private
		 * Asserts if the data structures in the flow are invalid.
		 */
		CONFIG::debug public function debugCheckTextFlow(validateControllers:Boolean=true):int
		{
			if (!Debugging.debugCheckTextFlow)
				return 0;
			
			var rslt:int = debugCheckFlowElement();
			
			if (_flowComposer && validateControllers)
			{
				var idx:int;
				var endPrevController:int = 0;
				for (idx = 0; idx < flowComposer.numControllers; idx++)
				{
					var controller:ContainerController = flowComposer.getControllerAt(idx);
					if (Debugging.verbose)
					{
						trace("controller:",idx,"absoluteStart:",controller.absoluteStart,"textLength:",controller.textLength);
					}
					
					rslt += assert(controller.absoluteStart == endPrevController, "controller has bad start");
					rslt += assert(controller.textLength >= 0, "controller has bad textLength");
					endPrevController = controller.absoluteStart+controller.textLength;
					rslt += assert(endPrevController <= textLength, "textLength may not extend past end of root element!");
				}
			}
			
			if (_flowComposer is StandardFlowComposer)
				rslt += StandardFlowComposer(_flowComposer).debugCheckTextFlowLines(validateControllers);
			return rslt;
		}
		
		/**
		 * Called after an import to validate that the entire flow textLength needs normalize.
		 * @private
		 */
		 CONFIG::debug public function debugCheckNormalizeAll():void
		{
			assert(normalizeStart == 0,"normalizeStart: bad normailzeStart");
			assert(normalizeLen == textLength,"debugCheckNormalizeAll: bad normalizeLen");
		}
		
		/**
		 * @copy flash.events.IEventDispatcher#addEventListener()
		 *
		 * @playerversion Flash 10
		 * @playerversion AIR 1.5
	 	 * @langversion 3.0
 		 */
 		 
		public function addEventListener(type:String, listener:Function, useCapture:Boolean = false, priority:int = 0, useWeakReference:Boolean = false): void
		{
			if (!_eventDispatcher)
				_eventDispatcher = new EventDispatcher(this);
			_eventDispatcher.addEventListener(type, listener, useCapture, priority, useWeakReference);
		}

		/**
		 * @copy flash.events.IEventDispatcher#dispatchEvent()
		 *
		 * @playerversion Flash 10
		 * @playerversion AIR 1.5
	 	 * @langversion 3.0
 		 */
 		 
		public function dispatchEvent(event:Event):Boolean
		{
			if (!_eventDispatcher)
				return true;
			return _eventDispatcher.dispatchEvent(event);
		}
		
		/**
		 * @copy flash.events.IEventDispatcher#hasEventListener()
		 *
		 * @playerversion Flash 10
		 * @playerversion AIR 1.5
	 	 * @langversion 3.0
 		 */
 		 
		public function hasEventListener(type:String):Boolean
		{
			if (!_eventDispatcher)
				return false;
			return _eventDispatcher.hasEventListener(type);
		}
		
		/**
		 * @copy flash.events.IEventDispatcher#removeEventListener().
		 * 
		 * @playerversion Flash 10
		 * @playerversion AIR 1.5
	 	 * @langversion 3.0
 		 */						
		public function removeEventListener(type:String, listener:Function, useCapture:Boolean = false): void
		{
			if (!_eventDispatcher)
				return;
			
			_eventDispatcher.removeEventListener(type, listener, useCapture);
		}

		/**
		 * @copy flash.events.IEventDispatcher#willTrigger()
		 * 
		 * @playerversion Flash 10
		 * @playerversion AIR 1.5
	 	 * @langversion 3.0
 		 */								
		public function willTrigger(type:String):Boolean
		{
			if (!_eventDispatcher)
				return false;
			return _eventDispatcher.willTrigger(type);
		}
		
		// elements which are images or blocks that *may* have children requiring activation or deactivation
		private var _elemsToUpdate:Dictionary;
		
		/** @private */
		tlf_internal function appendOneElementForUpdate(elem:FlowElement):void
		{
			if (_elemsToUpdate == null)
				_elemsToUpdate = new Dictionary();
			_elemsToUpdate[elem] = null;
		}
		
		/** @private */
		tlf_internal function mustUseComposer():Boolean
		{ 
			if (_interactiveObjectCount != 0)
				return true;

			if (_elemsToUpdate == null || _elemsToUpdate.length == 0)
				return false; 

			normalize();
			
			// anything that doesn't normalize completely forces use of the compser
			var rslt:Boolean = false;
			for (var elem:Object in _elemsToUpdate)
			{
				if ((elem as FlowElement).updateForMustUseComposer(this))
					rslt = true;
			}
			
			return rslt;
		}
		
		/** @private */
		tlf_internal function processModelChanged(changeType:String, elem:Object, changeStart:int, changeLen:int, needNormalize:Boolean, bumpGeneration:Boolean):void
		{
			// track elements that may need an update before the next compose
			if (elem is FlowElement)
				(elem as FlowElement).appendElementsForDelayedUpdate(this,changeType);
			
			if (bumpGeneration)
				_generation = _nextGeneration++;
			
			if (changeLen > 0 || changeType == ModelChange.ELEMENT_ADDED)
				damage(changeStart, changeLen, TextLineValidity.INVALID, needNormalize);
			
			if (formatResolver)
			{
				switch(changeType)
				{
					case ModelChange.ELEMENT_REMOVAL:
					case ModelChange.ELEMENT_ADDED:
					case ModelChange.STYLE_SELECTOR_CHANGED:
						formatResolver.invalidate(elem);
						elem.formatChanged(false);
						break;
				}
			}
		}
		
		/** 
		* The generation number for this TextFlow object. The undo and redo operations use the generation number to validate that 
		* it's legal to undo or redo an operation. The generation numbers must match. 
		* 
		* <p>Each model change increments <code>generation</code> so if the generation number changes, you know the 
		* TextFlow model has changed.</p>
		* 
		* @playerversion Flash 10
		* @playerversion AIR 1.5
	 	* @langversion 3.0
	 	*/
	 	
		public function get generation():uint
		{
			return _generation;
		}
		
		/** used to reset the number backwards after an undo or redo. @private */
		tlf_internal function setGeneration(num:uint):void
		{
			_generation = num;
		}
		
		/** @private */
		tlf_internal function processAutoSizeImageLoaded(elem:InlineGraphicElement):void
		{
			if (flowComposer)
				elem.appendElementsForDelayedUpdate(this,null);
		}
		
		/**
		 * Examine the damaged textLength of the TextFlow and put it in a normal form.  This includes adding spans to empty paragraph and
		 * merging sibling spans that have the same attributes.
		 * @private 
		 */
		tlf_internal function normalize():void
		{
			//trace("NORMALIZE");
			
			if (normalizeStart != -1)
			{
				var normalizeEnd:int = normalizeStart + (normalizeLen==0?1:normalizeLen);
				normalizeRange(normalizeStart==0?normalizeStart:normalizeStart-1,normalizeEnd);

				normalizeStart = -1;
				normalizeLen = 0;				
			}
			CONFIG::debug { debugCheckTextFlow(false); }
		}
		
		private var _hostFormatHelper:HostFormatHelper;
		
		/** The TextLayoutFormat object for this TextFlow object. This enables several optimizations for reusing 
		* host formats. For example;
		*
		* <listing>
		* textFlowA.hostFormat = textFlowB.hostFormat
		* </listing>
		* 
		* You must set format values before assigning the TextLayoutFormat object to <code>hostFormat</code>.
		* For example, the following lines do <em>not</em> set the font size to 24 because
		* the font size is set <em>after</em> the TextLayoutFormat object has been assigned to <code>hostFormat</code>.
		*
		* <listing>
		* format = new TextLayoutFormat()
		* textFlow.hostFormat = format
		* format.fontSize = 24;
		* </listing>
		*
		* @playerversion Flash 10
		* @playerversion AIR 1.5
	 	* @langversion 3.0
	 	*
	 	* @see flashx.textLayout.formats.ITextLayoutFormat ITextLayoutFormat
	 	*/
	 	
		public function get hostFormat():ITextLayoutFormat
		{ return _hostFormatHelper ? _hostFormatHelper.format : null; }
		public function set hostFormat(value:ITextLayoutFormat):void
		{
			if (value == null)
				_hostFormatHelper = null;
			else
			{
				if (_hostFormatHelper == null)
					_hostFormatHelper = new HostFormatHelper();
				_hostFormatHelper.format = value;
			}
			formatChanged();
		}
		
		/** @private */
		tlf_internal override function doComputeTextLayoutFormat():TextLayoutFormat
		{
			var parentPrototype:TextLayoutFormat = _hostFormatHelper ? _hostFormatHelper.getComputedPrototypeFormat() : null;
			return FlowElement.createTextLayoutFormatPrototype(formatForCascade,parentPrototype);
		}
		
		
		/** Use the formatResolver to get the character style of an Object.
		 * @param elem is either a FlowElement or a ContainerController(doesn't happen for characterformat)
		 * @return any styled CharacterFormat for that element
		 * @private
		 */
		tlf_internal function getTextLayoutFormatStyle(elem:Object):TextLayoutFormat
		{
			if (_formatResolver == null)
				return null;
			var rslt:ITextLayoutFormat = _formatResolver.resolveFormat(elem);
			if (rslt == null)
				return null;
			// optimization!
			var tlfvh:TextLayoutFormat = rslt as TextLayoutFormat;
			return tlfvh ? tlfvh : new TextLayoutFormat(rslt);
		}
		
		/** @private This API peeks at the background manager.  Use when removing data as things go out of view or are recomposed.  */
		tlf_internal function get backgroundManager():BackgroundManager
		{ return _backgroundManager; }
		
		/** @private */
		tlf_internal function clearBackgroundManager():void
		{ _backgroundManager = null; }
		
		/** @private.  Returns the existing backgroundManager - creating it if it doesn't exist.  Use when adding backgrounds to draw.  */
		tlf_internal function getBackgroundManager():BackgroundManager
		{
			if (!_backgroundManager && (flowComposer is StandardFlowComposer))
				_backgroundManager = (flowComposer as StandardFlowComposer).createBackgroundManager()
			return _backgroundManager;
		}
				
		/** A callback function for resolving element styles. You can use this to provide styling using CSS or 
		 * named styles, for example. 
		 *
		 * @playerversion Flash 10
		 * @playerversion AIR 1.5
	 	 * @langversion 3.0
	 	 *
	 	 * @see IFormatResolver
		 */
		 
		public function get formatResolver(): IFormatResolver
		{ 
			return _formatResolver; 
		}
		public function set formatResolver(val:IFormatResolver):void
		{
			if (_formatResolver != val)
			{
				if (_formatResolver)
					_formatResolver.invalidateAll(this);
				_formatResolver = val;
				if (_formatResolver)
					_formatResolver.invalidateAll(this);
					
				formatChanged(true);
			}
		}
		
		/** Invalidates all formatting information for the TextFlow, forcing it to be recomputed.
		 * Call this method when styles have changed.
		 *
		 * @playerversion Flash 10
		 * @playerversion AIR 1.5
		 * @langversion 3.0
		 *
	 	 * @see IFormatResolver#invalidateAll()
		 */
		 
		public function invalidateAllFormats():void
		{
			if (_formatResolver)
				_formatResolver.invalidateAll(this);
			formatChanged(true);
		}
	} // end TextFlow class
}
import flash.utils.Dictionary;

import flashx.textLayout.debug.assert;
import flashx.textLayout.elements.FlowElement;
import flashx.textLayout.formats.ITextLayoutFormat;
import flashx.textLayout.formats.TextLayoutFormat;
import flashx.textLayout.formats.TextLayoutFormat;
import flashx.textLayout.tlf_internal;

use namespace tlf_internal;

/** @private.  Expected usage is that all values are set. */
class HostFormatHelper
{
	private var _format:ITextLayoutFormat;
	private var _computedPrototypeFormat:TextLayoutFormat;
	
	public function get format():ITextLayoutFormat
	{ return _format; }
	public function set format(value:ITextLayoutFormat):void
	{ _format = value;  _computedPrototypeFormat = null; }

	public function getComputedPrototypeFormat():TextLayoutFormat
	{
		if (_computedPrototypeFormat == null)
		{
			var useFormat:ITextLayoutFormat;
			if (_format is TextLayoutFormat || _format is TextLayoutFormat)
				useFormat = _format;
			else
				useFormat = new TextLayoutFormat(_format);
			_computedPrototypeFormat = FlowElement.createTextLayoutFormatPrototype(useFormat,null);
		}
		return _computedPrototypeFormat;
	}		
		
}