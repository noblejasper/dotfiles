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
package flashx.textLayout.events
{
	import flash.display.DisplayObject;
	import flash.display.DisplayObjectContainer;
	import flash.display.Sprite;
	import flash.events.Event;
	import flash.events.EventDispatcher;
	import flash.events.KeyboardEvent;
	import flash.events.MouseEvent;
	import flash.geom.Matrix;
	import flash.geom.Point;
	import flash.geom.Rectangle;
	import flash.text.engine.TextBlock;
	import flash.text.engine.TextLine;
	import flash.text.engine.TextLineValidity;
	import flash.ui.Mouse;
	import flash.ui.MouseCursor;
	import flash.utils.Dictionary;
	
	import flashx.textLayout.compose.IFlowComposer;
	import flashx.textLayout.compose.TextFlowLine;
	import flashx.textLayout.container.ContainerController;
	import flashx.textLayout.debug.assert;
	import flashx.textLayout.elements.*;
	import flashx.textLayout.formats.BlockProgression;
	import flashx.textLayout.tlf_internal;
	import flashx.textLayout.utils.GeometryUtil;
	import flashx.textLayout.utils.HitTestArea;
	
	use namespace tlf_internal;
	
	[ExcludeClass]
	/**
	 * The ElementMouseEventManager class provides mouse element processing for visible
	 * FlowElements. The caller calls updateHitTests(), which traverses the FlowElement
	 * tree in the given character range, and collects hit test rectangles for FlowElements 
	 * with active event mirrors, and LinkElements. The latter class exposes a number of
	 * mouse event handlers that this class calls directly. If no character range is
	 * supplied, the class makes an educated guess about the visible character range by 
	 * inspecting the TextLine instances connected to the container widget as children.
	 * 
	 * In edit mode, moving the mouse over the element and clicking it should not activate
	 * it, but rather allow for editing. Passing in the value true as an argument
	 * to updateHitTests() requires the Ctrl key to be held down in order to pass mouse 
	 * events on the FlowElements. If the user presses or releases the Ctrl key while the
	 * mouse is over the container widget (and the pressing of the Ctrl key is required
	 * for activation, the class simulates mouseOut and mouseOver events to inform the 
	 * FlowElement underneath about the change. The class dispatches only FlowElementMouseEvents.
	 * 
	 * This class emits click events if the main mouse button is pressed and released over the
	 * same FlowElement.
	 * 
	 * The owner may choose to forward several mouse and keyboard events to this class so it
	 * does not need to listen to these events at the container itself. The constructor takes
	 * an array of event names that this class needs to listen to itself. The events that this
	 * class needs to listen to are MouseEvent.MOUSE_OVER, MouseEvent.MOUSE_OUT, MouseEvent.MOUSE_DOWN,
	 * MouseEvent.MOUSE_UP, MouseEvent.MOUSE_MOVE, KeyboardEvent.KEY_DOWN, and KeyboardEvent.KEY_UP.
	 * Note that MouseEvent.CLICK is not needed.
	 */
	public class FlowElementMouseEventManager
	{
		// The container that emits the mouse events, and that contains the TextLines as children.
		private var _container:DisplayObjectContainer;
		// the hit test rectangles if there is anything to hit test.
		private var _hitTests:HitTestArea = null;
		// The current element that has been identified as being hit.
		private var _currentElement:FlowElement = null;
		// The element that has received the last museDown event.
		private var _mouseDownElement:FlowElement = null;
		// If true, the Ctrl key is needed to send mouse events to a FlowElement.
		private var _needsCtrlKey:Boolean = false;
		// Reflects the state of the Ctrl key.
		private var _ctrlKeyState:Boolean = false;
		// The last mouse event serves as a template for fake mouse events (x, y, buttonDown)
		private var _lastMouseEvent:MouseEvent = null;
		/**
		 * @private
		 * Applies only if Ctrl key is needed: if the mouse entered the FlowElement
		 * with the button wodn and the Ctrl key down, ignore the state of the Ctrl
		 * key and treat it as not set. You do not want any reaction if the FlowElement
		 * was e.g. entered during a mark operation.
		 */
		private var _blockInteraction:Boolean = false;
		// Owner handles the event and calls dispatchEvent()
		private const OWNER_HANDLES_EVENT:int = 0;
		// This instance handles the event, but no event listener is registered.
		private const THIS_HANDLES_EVENT:int = 1;
		// This instance handles the event, and an an event listener is registered with the container/stage.
		private const THIS_LISTENS_FOR_EVENTS:int = 2;
		/** @private
		 * This object contains the event states that this class must
		 * register on its own behalf. The property name is the event name, 
		 * and the property value is one of the above three constants.
		 */
		private var _eventListeners:Object;
		/** @private
		 * This object contains objects that hold a bounding rectangle in its "rect",
		 * and a FlowElement in its "owner" property. The key is the toString() value
		 * of the rectangle. Every time updateHitTests() is called, the object is 
		 * checked for rectangles that still intersect with a given clip area, and 
		 * these rectangles are removed from the object before the new rectangles 
		 * are added. After that, the new hit test structure is built using this 
		 * object. The code that adds rectangles uses the key to store rectangles
		 * so updated rectangles are neatly overwritten.
		 */
		private var _hitRects:Object = null;
		/**
		 * The constructor needs the container that contains the TextLines to be checked
		 * for visible, clicked elements. The container is also used as the event dispatcher 
		 * for mouse events.
		 * 
		 * @param	container		The container holding the TextLines and emitting mouse events
		 * @param	eventNames		An array of event names that the owner supplies itself.
		 */
		public function FlowElementMouseEventManager(container:DisplayObjectContainer, eventNames:Array)
		{
			_container = container;
			_eventListeners = {};
			_eventListeners[MouseEvent.MOUSE_OVER] =
			_eventListeners[MouseEvent.MOUSE_OUT] =
			_eventListeners[MouseEvent.MOUSE_DOWN] =
			_eventListeners[MouseEvent.MOUSE_UP] =
			_eventListeners[MouseEvent.MOUSE_MOVE] =
			_eventListeners[KeyboardEvent.KEY_DOWN] =
			_eventListeners[KeyboardEvent.KEY_UP] = THIS_HANDLES_EVENT;
			// change the state for any event that the owner will supply
			for each (var name:String in eventNames)
				_eventListeners[name] = OWNER_HANDLES_EVENT;
		}
		
		/**
		 * Convert local mouse event coordinates, which are relative to the container
		 * or one of its children (the event's target is the current element) to
		 * container coordinates.
		 * 
		 * TODO: This is temporary code.
		 * 
		 * @param	evt				The mouse event containing the point to be converted.
		 * @return					A new Point instance containing the converted coordinates.
		 */
		public function mouseToContainer(evt:MouseEvent):Point
		{
			// we have to use localX and localY because the UnitTests generate
			// fake mouse events that are relative to a TextLine
			var obj:DisplayObject = evt.target as DisplayObject;
			CONFIG::debug { assert(obj != null, "Event target is not a DisplayObject"); }
			var containerPoint:Point = new Point(evt.localX, evt.localY);
			while (obj != _container)
			{
				var m:Matrix = obj.transform.matrix;
				containerPoint.offset(m.tx, m.ty);
				// TextLines sometimes do not have a parent for some reason
				obj = obj.parent;
				if (!obj)
					break;
			}
			return containerPoint;
		}
		
		/**
		 * Retrieve the status of the flag that controls whether the Ctrl key is needed
		 * to activate event generation.
		 */
		public function get needsCtrlKey():Boolean
		{
			return _needsCtrlKey;
		}
		
		/**
		 * Set the status of the flag that controls whether the Ctrl key is needed
		 * to activate event generation.
		 */
		public function set needsCtrlKey(k:Boolean):void
		{
			_needsCtrlKey = k;
		}
		
		/**
		 * Create an array of all FlowElements that are currently visible and that have an active
		 * event mirror, plus all LinkElements by default, and update the hit test area for these
		 * FlowElements. Clip the elements against the given clipping rectangle. All coordinates 
		 * are assumed to be container coordinates.
		 * 
		 * <p>If the start index is -1 and or the end index is -1, the method attempts to make an educated 
		 * guess about the visible part of the text by inspecting the visible TextLine instances, and
		 * using their textBlockBeginIndex values as character offsets into the TextFlow tree. Note that
		 * this method may be slow if the container contains an entire tree of DisplayObjects, because
		 * the tree must be scanned recursively. It is always better to supply the start and end positions.</p>
		 * 
		 * @param	clipRect		The clipping rectangle, in container coordinates.
		 * @param	textFlow		The TextFlow instance containing the elements to be tracked
		 * @param	startPos		The character start position. If -1, the method attempts to
		 * 							determine the first visible character.
		 * @param	endPos			The character end position. If -1, the method attempts to
		 * 							determine the last visible character.
		 * @param	needsCtrlKey	If true, the event handler does not emit events unless
		 * 							the Ctrl key is down. If the text is editable, simple clicks 
		 * 							and moves should remain in the container rather than be
		 * 							forwarded to the element.
		 */
		public function updateHitTests(xoffset:Number, clipRect:Rectangle, 
									  textFlow:TextFlow,
									  startPos:int, endPos:int, 
									  needsCtrlKey:Boolean=false):void
		{
			_needsCtrlKey = needsCtrlKey;
			
			var rect:Rectangle;
			var obj:Object;
			
			var elements:Array = [];
			if (textFlow.interactiveObjectCount != 0 && startPos != endPos)	// check for empty container
			{
				collectElements(textFlow, startPos, endPos, elements);
				CONFIG::debug
				{
					if (elements.length)
					{
						for each (var elem:FlowElement in elements)
						{
							// trace(startPos,endPos,elem.defaultTypeName,elem.getAbsoluteStart(),elem.textLength);
							assert(elem.getAbsoluteStart() < endPos,"updateHitTests bad absoluteStart");
							assert(elem.getAbsoluteStart()+elem.textLength >= startPos,"updateHitTests bad absoluteEnd"); 
						}
					}
				}
			}
			
			var newHitRects:Object;
			var rectCount:int = 0;
			
			if (elements.length != 0)
			{		
				newHitRects = {};
	
				for each (var element:FlowElement in elements)
				{
					var elemStart:int = element.getAbsoluteStart();
					var elemEnd:int = Math.min(elemStart + element.textLength, endPos);
					var elemRects:Array = GeometryUtil.getHighlightBounds(new TextRange(element.getTextFlow(), elemStart, elemEnd));
					// this is an array of objects with textLine and rect properties
					// Create an array of rectangle and owner objects to feed into a HitTestArea
					for each (obj in elemRects)
					{
						rect = obj.rect;
						rect.x = clipRect.x + obj.textLine.x + rect.x + xoffset;
						rect.y = clipRect.y + obj.textLine.y + rect.y;
						// Only use the visible parts of the rectangle if any
						rect = rect.intersection(clipRect);
						if (!rect.isEmpty())
						{
							// use integer rectangles for better toString() representation
							// this decreases the number of stored rectangles due to FP errors
							rect.x = int(rect.x);
							rect.y = int(rect.y);
							rect.width = int(rect.width);
							rect.height = int(rect.height);
							var name:String = rect.toString();
							var oldObj:Object = newHitRects[name];
							if (!oldObj || oldObj.owner != element)
							{
								// replace or add operation
								newHitRects[name] = { rect: rect, owner: element };
								rectCount++;
							}
						}
					}
				}
			}

			if (rectCount > 0)
			{
				if (!_hitTests)
					startHitTests();
				_hitRects = newHitRects;
				_hitTests = new HitTestArea(newHitRects);
			}
			else
				stopHitTests();
		}
		
		/** @private Start hit testing. */
		tlf_internal function startHitTests():void
		{
			_currentElement = null;
			_mouseDownElement = null;
			_ctrlKeyState = false;
			// conditionally attach required event listeners
			addEventListener(MouseEvent.MOUSE_OVER, false);
			addEventListener(MouseEvent.MOUSE_OUT, false);
			addEventListener(MouseEvent.MOUSE_DOWN, false);
			addEventListener(MouseEvent.MOUSE_UP, false);
			addEventListener(MouseEvent.MOUSE_MOVE, false);
		}
		
		/**
		 * Stop hit testing altogether. You must call this method if the
		 * FlowElement structure of the visible area has changed; you should
		 * also call this method if the visible area is rebuilt completely.
		 */
		public function stopHitTests():void
		{
			// conditionally remove required event listeners
			removeEventListener(MouseEvent.MOUSE_OVER, false);
			removeEventListener(MouseEvent.MOUSE_OUT, false);
			removeEventListener(MouseEvent.MOUSE_DOWN, false);
			removeEventListener(MouseEvent.MOUSE_UP, false);
			removeEventListener(MouseEvent.MOUSE_MOVE, false);
			removeEventListener(KeyboardEvent.KEY_DOWN, true);
			removeEventListener(KeyboardEvent.KEY_UP, true);
			_hitRects = null;
			_hitTests = null;
			_currentElement = null;
			_mouseDownElement = null;
			_ctrlKeyState = false;
		}
		
		private function addEventListener(name:String, kbdEvent:Boolean = false):void
		{
			if (_eventListeners[name] === THIS_HANDLES_EVENT)
			{
				var target:DisplayObjectContainer;
				var listener:Function;
				if (kbdEvent)
				{
					target = _container.stage;
					if (!target)
						target = _container;
					listener = hitTestKeyEventHandler;
				}
				else
				{
					target = _container;
					listener = hitTestMouseEventHandler;
				}
				target.addEventListener(name, listener, false, 1);
				_eventListeners[name] = THIS_LISTENS_FOR_EVENTS;
			}
		}
		
		private function removeEventListener(name:String, kbdEvent:Boolean):void
		{
			if (_eventListeners[name] === THIS_LISTENS_FOR_EVENTS)
			{
				var target:DisplayObjectContainer;
				var listener:Function;
				if (kbdEvent)
				{
					target = _container.stage;
					if (!target)
						target = _container;
					listener = hitTestKeyEventHandler;
				}
				else
				{
					target = _container;
					listener = hitTestMouseEventHandler;
				}
				target.removeEventListener(name, listener);
				_eventListeners[name] = THIS_HANDLES_EVENT;
			}
		}

		/** @private
		 * Collect all FlowElements with an active event mirror plus all LinkElements in to the given
		 * array. This method is recursive so it can iterate over child FlowElements.
		 * 
		 * @param	parent		The parent element to scan.
		 * @param	startPosition	The starting character position.
		 * @param	endPosition	The ending character position.
		 * @param	results		The array to fill with FlowElements that match.
		 */
		tlf_internal function collectElements(parent:FlowGroupElement, startPosition:int, endPosition:int, results:Array):void
		{
			CONFIG::debug { assert(startPosition >= 0,"Bad startPosition parameter"); }
			CONFIG::debug { assert(endPosition >= startPosition,"Bad endPosition parameter"); }

			var i:int = parent.findChildIndexAtPosition(startPosition);
			for (; i < parent.numChildren; i++)
			{
				var child:FlowElement = parent.getChildAt(i);
				if (child.parentRelativeStart >= endPosition)
					// behind end
					break;
				if (child.hasActiveEventMirror() || (child is LinkElement))
					results.push(child);
				var group:FlowGroupElement = child as FlowGroupElement;
				if (group)
					collectElements(group, Math.max(startPosition-group.parentRelativeStart,0), endPosition-group.parentRelativeStart, results);
			}
		}
		
		/**
		 * Dispatch the mouse and keyboard events that the owner sends.
		 */
		public function dispatchEvent(evt:Event):void
		{
			var mouseEvt:MouseEvent = evt as MouseEvent;
			if (mouseEvt)
				hitTestMouseEventHandler(mouseEvt);
			else
			{
				var keyEvt:KeyboardEvent = evt as KeyboardEvent;
				if (keyEvt)
					hitTestKeyEventHandler(keyEvt);
			}
		}
		
		/** @private
		 * Process the key down/key up messages for the Ctrl key. This handler
		 * is required to make a visual change for the element if the Ctrl key is
		 * pressed or released while not moving the mouse. If a Ctrl key change
		 * is detected, and the owner signalled that the Ctrl key is required,
		 * there will be a mouseOver element if the Ctrl key is pressed, and a
		 * mouseOut event if the Ctrl key is released.
		 */
		private function hitTestKeyEventHandler(evt:KeyboardEvent):void
		{
			if (!_blockInteraction)
				checkCtrlKeyState(evt.ctrlKey);
		}
		
		private function checkCtrlKeyState(curState:Boolean):void
		{
			// currently, support only LinkElements
			var link:LinkElement = _currentElement as LinkElement;
			// do nothing if the Ctrl key is not needed, or the key state did not change
			// we also need the last mouse event for the button state to pass in to the link
			if(!link || !_needsCtrlKey || !_lastMouseEvent || (curState == _ctrlKeyState))
				return;
			_ctrlKeyState = curState;

			// the event type is irrelevant here - only the buttonDown state is relevant
			if (_ctrlKeyState)
				link.mouseOverHandler(this, _lastMouseEvent);
			else
				link.mouseOutHandler(this, _lastMouseEvent);
		}
		
		/** @private
		 * Process mouse events.
		 * 
		 * For event mirroring, hasActiveEventMirror() must be true; if so, the event mirror
		 * dispatches a FlowElementMouseEvent. The method generates fake rollOut and rollOver
		 * events; the original mouseOut and mouseOver events are container and/or TextLine events.
		 */		
		private function hitTestMouseEventHandler(evt:MouseEvent):void
		{
			if (!_hitTests)
				return;
			
			// note that mouseOver and mouseOut are used for hit-testing only
			// need the last mouse event's button state to pass in to LinkElement
			// in case the state of the Ctrl key changes (see hitTestKeyEventHandler())
			_lastMouseEvent = evt;
			
			var containerPoint:Point = mouseToContainer(evt);
			var hitElement:FlowElement = _hitTests.hitTest(containerPoint.x, containerPoint.y);
			if (hitElement != _currentElement)
			{
				_mouseDownElement = null;
				if (_currentElement)
					// generate a mouseOut event
					localDispatchEvent(FlowElementMouseEvent.ROLL_OUT, evt);
				else if (evt.buttonDown)
					// do not interact if the button is down to not disturb e.g. 
					// a mark operation in the container
					_blockInteraction = true;
				_currentElement = hitElement;
				if (_currentElement)
					// generate a mouseOver event
					localDispatchEvent(FlowElementMouseEvent.ROLL_OVER, evt);
				else
					// no FlowElement underneathmouse: reset interaction blocker
					_blockInteraction = false;
			}

			var isClick:Boolean = false;
			var eventType:String = null;
			switch (evt.type)
			{
				case MouseEvent.MOUSE_MOVE:
					eventType = FlowElementMouseEvent.MOUSE_MOVE;
					// Need to check the state of the event's Ctrl key in case
					// the container lost focus, and the mouse was moved
					if (!_blockInteraction)
						checkCtrlKeyState(evt.ctrlKey);
					break;
				case MouseEvent.MOUSE_DOWN: 
					_mouseDownElement = _currentElement;
					eventType = FlowElementMouseEvent.MOUSE_DOWN;
					break;
				case MouseEvent.MOUSE_UP:
					eventType = FlowElementMouseEvent.MOUSE_UP;
					isClick = (_currentElement == _mouseDownElement);
					_mouseDownElement = null;
					break;
			}
			
			if (_currentElement && eventType)
			{
				localDispatchEvent(eventType, evt);
				if (isClick)
					localDispatchEvent(FlowElementMouseEvent.CLICK, evt);
			}
		}
		
		/** @private
		 * Dispatch a FlowElementMouseEvent with the given type. First, attempt to
		 * dispatch to an event mirror if attached and listening. If there was nobody
		 * listening at the event mirror, or the event did not stop propagation, dispatch
		 * the event to the TextFlow as well.
		 * 
		 * @param	type			The event type, should be a constant defind in FlowElementMouseEvent.
		 * @param	originalEvent	The original mouse event.
		 * @return					true if the event was dispatched and shgould not be distributed further.
		 */
		tlf_internal function dispatchFlowElementMouseEvent(type:String, originalEvent:MouseEvent):Boolean
		{
			// Mimick old behavior, and emit only rollOut events if Ctrl key is not down
			if (_needsCtrlKey && !originalEvent.ctrlKey && type != FlowElementMouseEvent.ROLL_OUT)
				return false;
			
			var locallyListening:Boolean = _currentElement.hasActiveEventMirror();
			var textFlow:TextFlow = _currentElement.getTextFlow();
			var textFlowListening:Boolean = false;
			if (textFlow)
				textFlowListening = textFlow.hasEventListener(type);
			if (!locallyListening && !textFlowListening)
			{
				return false;
			}
			
			var event:FlowElementMouseEvent = new FlowElementMouseEvent(type, false, true, _currentElement, originalEvent);
			if (locallyListening)
			{
				_currentElement.getEventMirror().dispatchEvent(event);
				if (event.isDefaultPrevented())
					return true;
			}
			if (textFlowListening)
			{
				textFlow.dispatchEvent(event);
				if (event.isDefaultPrevented())
					return true;
			}
			return false;
		}
		
		/** @private
		 * Dispatch a FlowElementMouseEvent, and call the correct LinkElement
		 * event handler if the current element is a LinkElement.
		 * 
		 * LinkElements implement several mouse handlers; these are called directly
		 * so LinkElements do not needs to register themselves with their own 
		 * event mirror.
		 */
		
		private function localDispatchEvent(type:String, evt:MouseEvent):void
		{
			if (_blockInteraction || !_currentElement)
				return;
			
			// Attach or detach listeners for the Ctrl key if needed
			if (_needsCtrlKey)
				switch (type)
			{
				case FlowElementMouseEvent.ROLL_OVER:
					addEventListener(KeyboardEvent.KEY_DOWN, true);
					addEventListener(KeyboardEvent.KEY_UP, true);
					break;
				case FlowElementMouseEvent.ROLL_OUT:
					removeEventListener(KeyboardEvent.KEY_DOWN, true);
					removeEventListener(KeyboardEvent.KEY_UP, true);
					break;
			}
			
			if (dispatchFlowElementMouseEvent(type, evt))
				return;
			
			// dispatch to a LinkElement only if Ctrl key conditions fit
			var link:LinkElement = (!_needsCtrlKey || evt.ctrlKey) ? (_currentElement as LinkElement) : null;
			if (!link)
				return;
			
			// use the FlowElementMouseEvent type - the mouse event type may be unrelated
			switch (type)
			{
				case FlowElementMouseEvent.MOUSE_DOWN: 
					link.mouseDownHandler(this, evt); 
					break;
				case FlowElementMouseEvent.MOUSE_MOVE:
					link.mouseMoveHandler(this, evt); 
					break;
				case FlowElementMouseEvent.ROLL_OUT:
					link.mouseOutHandler(this, evt); 
					break;
				case FlowElementMouseEvent.ROLL_OVER:
					link.mouseOverHandler(this, evt); 
					break;
				case FlowElementMouseEvent.MOUSE_UP:
					link.mouseUpHandler(this, evt); 
					break;
				case FlowElementMouseEvent.CLICK:
					link.mouseClickHandler(this, evt);
					break;
			}
		}

		/** @private
		 * Utility method for LinkElement (and other elements in the future that
		 * might implement the same mouse handlers) to set and reset the Hand cursor.
		 */
		tlf_internal function setHandCursor(state:Boolean=true):void
		{
			if (_currentElement == null)
				return;
			
			var tf:TextFlow = _currentElement.getTextFlow();
			if (tf != null && tf.flowComposer && tf.flowComposer.numControllers)
			{
				var sprite:Sprite = _container as Sprite;
				if (sprite)
				{
					sprite.buttonMode = state;
					sprite.useHandCursor = state;
				}
				if (state)
					Mouse.cursor = MouseCursor.BUTTON;
				else
				{
					var wmode:String = tf.computedFormat.blockProgression;									
					if (tf.interactionManager && (wmode != BlockProgression.RL))
						Mouse.cursor = MouseCursor.IBEAM;
					else
						Mouse.cursor = MouseCursor.AUTO;
				}
				Mouse.hide();
				Mouse.show();
			}
		}
	}
}
