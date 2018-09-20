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
	import flash.display.BlendMode;
	import flash.display.Sprite;
	import flash.system.Capabilities;
	
	import flashx.textLayout.compose.StandardFlowComposer;
	import flashx.textLayout.edit.SelectionFormat;
	import flashx.textLayout.formats.FormatValue;
	import flashx.textLayout.formats.IListMarkerFormat;
	import flashx.textLayout.formats.ITextLayoutFormat;
	import flashx.textLayout.formats.ListMarkerFormat;
	import flashx.textLayout.formats.TextDecoration;
	import flashx.textLayout.formats.TextLayoutFormat;
	import flashx.textLayout.tlf_internal;
	
	use namespace tlf_internal;
	
	/** 
	* The Configuration class is a primary point of integration between the Text Layout Framework and an application. You can 
	* include a Configuration object as a parameter to the <code>TextFlow()</code> constructor when you create a new TextFlow
	* instance. It allows the application to initially control how the Text Layout Framework behaves.
	* 
	* <p>The Configuration class allows you to specify initial, paragraph and container formats for the text flow 
	* through the <code>textFlowInitialFormat</code> property. It also allows you to specify initial format attributes for links, selection,
	* scrolling, and for handling the Tab and Enter keys.</p>
	*
	* @includeExample examples\ConfigurationExample.as -noswf
	*
	* @playerversion Flash 10
	* @playerversion AIR 1.5
	* @langversion 3.0
	* 
	* @see flashx.textLayout.formats.ITextLayoutFormat ITextLayoutFormat
	* @see flashx.textLayout.edit.SelectionFormat SelectionFormat
	* @see TextFlow
	*/
	
	public class Configuration implements IConfiguration
	{
		/** @private */
		static tlf_internal function versionIsAtLeast(major:int,minor:int):Boolean
		{ 
			var versionData:Array = Capabilities.version.split(" ")[1].split(","); 
			return int(versionData[0]) > major || (int(versionData[0]) == major && int(versionData[1]) >= minor);
		}
		
		/** @private The player may disable the feature for older swfs.  */
		static tlf_internal const playerEnablesArgoFeatures:Boolean = versionIsAtLeast(10,1); 
		
		/** @private The player may disable the feature for older swfs, so its not enough to check
		the Player version number, the SWF must also be marked as a version 11 SWF to use Spicy features.  */
		static tlf_internal const playerEnablesSpicyFeatures:Boolean = versionIsAtLeast(10,2) && (new Sprite).hasOwnProperty("needsSoftKeyboard"); 
		static tlf_internal const hasTouchScreen:Boolean = playerEnablesArgoFeatures && Capabilities["touchScreenType"] != "none";
		
		/** If manageTabKey and manageEnterKey are false, the client must handle those keys on their own. */
		private var _manageTabKey:Boolean;
		private var _manageEnterKey:Boolean;
		
		private var _overflowPolicy:String;
		
		private var _enableAccessibility:Boolean;
		private var _releaseLineCreationData:Boolean;
		
		private var _defaultLinkNormalFormat:ITextLayoutFormat;
		private var _defaultLinkActiveFormat:ITextLayoutFormat;
		private var _defaultLinkHoverFormat:ITextLayoutFormat;
		
		private var _defaultListMarkerFormat:IListMarkerFormat;
		
		private var _textFlowInitialFormat:ITextLayoutFormat;
		
		private var _focusedSelectionFormat:SelectionFormat;
		private var _unfocusedSelectionFormat:SelectionFormat;
		private var _inactiveSelectionFormat:SelectionFormat;	
		
		// scrolling vars
		private var _scrollDragDelay:Number;
		private var _scrollDragPixels:Number;
		private var _scrollPagePercentage:Number;
		private var _scrollMouseWheelMultiplier:Number;
		
		private var _flowComposerClass:Class;
		private var _inlineGraphicResolverFunction:Function;
				
		/** Constructor - creates a default configuration. 
		*
		* @param initializeWithDefaults Specifies whether to initialize the configuration with
		* the default values. Default is <code>true</code>. If set to <code>false</code>, initializes
		* without default values, thereby saving some objects. The <code>clone()</code> method sets this
		* to <code>false</code> and copies the properties from the original object.
		*
		* @playerversion Flash 10
		* @playerversion AIR 1.5
	 	* @langversion 3.0
	 	* 
		* @see flashx.textLayout.edit.SelectionFormat SelectionFormat
		* @see flashx.textLayout.compose.StandardFlowComposer StandardFlowComposer
		*/
		public function Configuration(initializeWithDefaults:Boolean = true)
		{
			if (initializeWithDefaults)
				initialize()
		}
		
		private function initialize():void
		{
			var scratchFormat:TextLayoutFormat;
	
			_manageTabKey = false;
			_manageEnterKey = true;
			_overflowPolicy = OverflowPolicy.FIT_DESCENDERS;
			_enableAccessibility = false;
			_releaseLineCreationData = false;
			
			_focusedSelectionFormat = new SelectionFormat(0xffffff, 1.0, BlendMode.DIFFERENCE);
			_unfocusedSelectionFormat = new SelectionFormat(0xffffff, 0, BlendMode.DIFFERENCE, 0xffffff, 0.0, BlendMode.DIFFERENCE, 0);
			_inactiveSelectionFormat  = _unfocusedSelectionFormat;
				
			scratchFormat = new TextLayoutFormat();
			scratchFormat.textDecoration = TextDecoration.UNDERLINE;
			scratchFormat.color = 0x0000FF;//default link color is blue
			_defaultLinkNormalFormat = scratchFormat;
			
			var listMarkerFormat:ListMarkerFormat = new ListMarkerFormat();
			listMarkerFormat.paragraphEndIndent = 4;
			_defaultListMarkerFormat = listMarkerFormat;
				
			scratchFormat = new TextLayoutFormat();
			scratchFormat.lineBreak = FormatValue.INHERIT;
			scratchFormat.paddingLeft = FormatValue.INHERIT;
			scratchFormat.paddingRight = FormatValue.INHERIT;
			scratchFormat.paddingTop = FormatValue.INHERIT;
			scratchFormat.paddingBottom = FormatValue.INHERIT;
			scratchFormat.verticalAlign = FormatValue.INHERIT;
			scratchFormat.columnCount = FormatValue.INHERIT;
			scratchFormat.columnCount = FormatValue.INHERIT;
			scratchFormat.columnGap = FormatValue.INHERIT;
			scratchFormat.columnWidth = FormatValue.INHERIT;
			_textFlowInitialFormat = scratchFormat;
					
			_scrollDragDelay = 35;
			_scrollDragPixels = 20;
			_scrollPagePercentage = 7.0/8.0;
			_scrollMouseWheelMultiplier = 20;
				
			_flowComposerClass = StandardFlowComposer;
		}
		
		private var _immutableClone:IConfiguration;
		
		/** TextFlows are configured with an immutable clone of a Configuration.  Once a TextFlow is create it uses an immutable configuration. @private */
		tlf_internal function getImmutableClone():IConfiguration
		{
			if (!_immutableClone)
			{
				var clonedConifg:Configuration = clone();
				_immutableClone = clonedConifg;
				// an immutable clone is its own immutable clone
				clonedConifg._immutableClone = clonedConifg;
			}
			return _immutableClone; 
		}
		
		/** Creates a clone of the Configuration object.
		*
		* @playerversion Flash 10
		* @playerversion AIR 1.5
	 	* @langversion 3.0
	 	*/
		public function clone():Configuration
		{
			var config:Configuration = new Configuration(false);
			// must copy all values
			config.defaultLinkActiveFormat = defaultLinkActiveFormat;
			config.defaultLinkHoverFormat  = defaultLinkHoverFormat;
			config.defaultLinkNormalFormat = defaultLinkNormalFormat;
			config.defaultListMarkerFormat = defaultListMarkerFormat;
			config.textFlowInitialFormat = _textFlowInitialFormat;
			config.focusedSelectionFormat = _focusedSelectionFormat;
			config.unfocusedSelectionFormat = _unfocusedSelectionFormat;
			config.inactiveSelectionFormat = _inactiveSelectionFormat;
			
			config.manageTabKey = _manageTabKey;
			config.manageEnterKey = _manageEnterKey;
			config.overflowPolicy = _overflowPolicy;
			config.enableAccessibility = _enableAccessibility;
			config.releaseLineCreationData = _releaseLineCreationData;
			
			config.scrollDragDelay = _scrollDragDelay;
			config.scrollDragPixels = _scrollDragPixels;
			config.scrollPagePercentage = _scrollPagePercentage;
			config.scrollMouseWheelMultiplier = _scrollMouseWheelMultiplier;
			
			config.flowComposerClass = _flowComposerClass;
			config._inlineGraphicResolverFunction = _inlineGraphicResolverFunction;
			return config; 
		}
		
		/** @copy IConfiguration#manageTabKey
		*
		* @playerversion Flash 10
		* @playerversion AIR 1.5
	 	* @langversion 3.0
		*/
		
		public function get manageTabKey():Boolean
		{ return _manageTabKey; }
		public function set manageTabKey(val:Boolean):void
		{ _manageTabKey = val; _immutableClone = null; }

		/** 
		* @copy IConfiguration#manageEnterKey
		*
		* @playerversion Flash 10
		* @playerversion AIR 1.5
	 	* @langversion 3.0
		*/
		
		public function get manageEnterKey():Boolean
		{ return _manageEnterKey; }
		public function set manageEnterKey(val:Boolean):void
		{ _manageEnterKey = val; _immutableClone = null; }
		

		
		/** 
		* @copy IConfiguration#overflowPolicy
		*
		* @playerversion Flash 10
		* @playerversion AIR 1.5
	 	* @langversion 3.0
	 	*
		* @see OverflowPolicy
		*/

		public function get overflowPolicy():String
		{ 	return _overflowPolicy; }
		public function set overflowPolicy(value:String):void
		{ 	_overflowPolicy = value; }
				
		/** 
		* @copy IConfiguration#defaultLinkNormalFormat
		*
		* @playerversion Flash 10
		* @playerversion AIR 1.5
	 	* @langversion 3.0
	 	*
		* @see FlowElement#linkNormalFormat
		* @see flashx.textLayout.formats.ITextLayoutFormat ITextLayoutFormat
		* @see LinkElement
		*/
		
		public function get defaultLinkNormalFormat():ITextLayoutFormat
		{ return _defaultLinkNormalFormat; }
		public function set defaultLinkNormalFormat(val:ITextLayoutFormat):void
		{ _defaultLinkNormalFormat = val; _immutableClone = null; }

		/** 
		 * @copy IConfiguration#defaultListMarkerFormat
		 *
		 * @playerversion Flash 10
		 * @playerversion AIR 1.5
		 * @langversion 3.0
		 *
		 * @see FlowElement#listMarkerFormat
		 * @see flashx.textLayout.formats.IListMarkerFormat IListMarkerFormat
		 * @see LinkElement
		 */
		
		public function get defaultListMarkerFormat():IListMarkerFormat
		{ return _defaultListMarkerFormat; }
		public function set defaultListMarkerFormat(val:IListMarkerFormat):void
		{ _defaultListMarkerFormat = val; _immutableClone = null; }
		
		/** 
		* @copy IConfiguration#defaultLinkHoverFormat  
		*
		* @playerversion Flash 10
		* @playerversion AIR 1.5
	 	* @langversion 3.0
	 	*
		* @see  FlowElement#linkHoverFormat
		* @see flashx.textLayout.formats.ITextLayoutFormat ITextLayoutFormat
		* @see LinkElement
		*/
		
		public function get defaultLinkHoverFormat():ITextLayoutFormat
		{ return _defaultLinkHoverFormat; }	
		public function set defaultLinkHoverFormat(val:ITextLayoutFormat):void
		{ _defaultLinkHoverFormat = val; _immutableClone = null; }
			
		/** 
		* @copy IConfiguration#defaultLinkActiveFormat
		*
		* @playerversion Flash 10
		* @playerversion AIR 1.5
	 	* @langversion 3.0
	 	*
		* @see FlowElement#linkActiveFormat 
		* @see flashx.textLayout.formats.ITextLayoutFormat ITextLayoutFormat
		* @see LinkElement
		*/
		
		public function get defaultLinkActiveFormat():ITextLayoutFormat
		{ return _defaultLinkActiveFormat; }
		public function set defaultLinkActiveFormat(val:ITextLayoutFormat):void
		{ _defaultLinkActiveFormat = val; _immutableClone = null; }
		
		/** 
		* @copy IConfiguration#textFlowInitialFormat
		*
		* @playerversion Flash 10
		* @playerversion AIR 1.5
	 	* @langversion 3.0
	 	*
		* @see TextFlow
		* @see flashx.textLayout.formats.ITextLayoutFormat ITextLayoutFormat
		*/
		
		public function get textFlowInitialFormat():ITextLayoutFormat
		{ return _textFlowInitialFormat; }
		public function set textFlowInitialFormat(val:ITextLayoutFormat):void
		{ _textFlowInitialFormat = val; _immutableClone = null; }
		

		
		/** 
		* @copy IConfiguration#focusedSelectionFormat 
		*
		* @playerversion Flash 10
		* @playerversion AIR 1.5
	 	* @langversion 3.0
	 	*
	 	* @see flashx.textLayout.edit.SelectionFormat SelectionFormat
		* @see flashx.textLayout.edit.SelectionManager#focusedSelectionFormat SelectionManager.focusedSelectionFormat
		* @see TextFlow
		*/
		
		public function get focusedSelectionFormat():SelectionFormat
		{ return _focusedSelectionFormat; }
		public function set focusedSelectionFormat(val:SelectionFormat):void
		{	if (val != null)
			{	
				_focusedSelectionFormat = val; 
				_immutableClone = null;
			} 
		}
		
		/** 
		* @copy IConfiguration#unfocusedSelectionFormat
		*
		* @playerversion Flash 10
		* @playerversion AIR 1.5
	 	* @langversion 3.0
	 	*
	 	* @see flashx.textLayout.edit.SelectionFormat SelectionFormat
		* @see flashx.textLayout.edit.SelectionManager#unfocusedSelectionFormat SelectionManager.unfocusedSelectionFormat
		* @see TextFlow
		*/
		
		public function get unfocusedSelectionFormat():SelectionFormat
		{ return _unfocusedSelectionFormat; }
		public function set unfocusedSelectionFormat(val:SelectionFormat):void
		{	if (val != null)
			{	
				_unfocusedSelectionFormat = val; 
				_immutableClone = null;
			} 
		}		
		
		/** 
		* @copy IConfiguration#inactiveSelectionFormat
		*
		* @playerversion Flash 10
		* @playerversion AIR 1.5
	 	* @langversion 3.0
	 	*
	 	* @see flashx.textLayout.edit.SelectionFormat SelectionFormat
		* @see flashx.textLayout.edit.SelectionManager#inactiveSelectionFormat SelectionManager.inactiveSelectionFormat
		* @see TextFlow
		*/
		
		public function get inactiveSelectionFormat():SelectionFormat
		{ return _inactiveSelectionFormat; }
		public function set inactiveSelectionFormat(val:SelectionFormat):void
		{	
			if (val != null)
			{
				_inactiveSelectionFormat = val; 
				_immutableClone = null; 
			}
		}												
		
		/** 
		* @copy IConfiguration#scrollDragDelay
		*
		* @playerversion Flash 10
		* @playerversion AIR 1.5
	 	* @langversion 3.0
		*/
		
		public function get scrollDragDelay():Number
		{ return _scrollDragDelay; }
		public function set scrollDragDelay(val:Number):void
		{
			if (val > 0) {
				_scrollDragDelay = val;
				_immutableClone = null;
			}
		}
		
		/** 
		* @copy IConfiguration#scrollDragPixels
		*
		* @playerversion Flash 10
		* @playerversion AIR 1.5
	 	* @langversion 3.0
		*/
		
		public function get scrollDragPixels():Number
		{ return _scrollDragPixels; }
		public function set scrollDragPixels(val:Number):void
		{
			if (val > 0) {
				_scrollDragPixels = val;
				_immutableClone = null;
			}
		}

		/**
		* @copy IConfiguration#scrollPagePercentage
		*
		* @playerversion Flash 10
		* @playerversion AIR 1.5
	 	* @langversion 3.0
		*/
		
		public function get scrollPagePercentage(): Number
		{ return _scrollPagePercentage; }
		public function set scrollPagePercentage(val:Number):void
		{
			if (val > 0) {
				_scrollPagePercentage = val;
				_immutableClone = null;
			}
		}
		
		/** 
		* @copy IConfiguration#scrollMouseWheelMultiplier
		*
		* @playerversion Flash 10
		* @playerversion AIR 1.5
	 	* @langversion 3.0
		*/
		
		public function get scrollMouseWheelMultiplier(): Number
		{ return _scrollMouseWheelMultiplier; }
		public function set scrollMouseWheelMultiplier(val:Number):void
		{
			if (val > 0) {
				_scrollMouseWheelMultiplier = val;
				_immutableClone = null;
			}
		}
		
		/** 
		* @copy IConfiguration#flowComposerClass
		*
		* @playerversion Flash 10
		* @playerversion AIR 1.5
	 	* @langversion 3.0
	 	*
	 	* @see flashx.textLayout.compose.StandardFlowComposer StandardFlowComposer
		* @see flashx.textLayout.elements.TextFlow TextFlow
		*/
		
		public function get flowComposerClass(): Class
		{ return _flowComposerClass; }
		public function set flowComposerClass(val:Class):void
		{
			_flowComposerClass = val;
			_immutableClone = null;
		}
	
		/** 
		* @copy IConfiguration#enableAccessibility
		*
		* @playerversion Flash 10
		* @playerversion AIR 1.5
	 	* @langversion 3.0
	 	*
	 	* @see TextFlow
		*/
		
		public function get enableAccessibility():Boolean
		{ return _enableAccessibility; }
		public function set enableAccessibility(val:Boolean):void
		{
			_enableAccessibility = val;
			_immutableClone = null;
		}
		
		/** 
		* @copy IConfiguration#releaseLineCreationData
		* 
		* @playerversion Flash 10
		* @playerversion AIR 1.5
		* @langversion 3.0
		*
		* @see flashx.textLayout.compose.StandardFlowComposer StandardFlowComposer
		* @see flash.text.engine.TextBlock#releaseLineCreationData() TextBlock.releaseLineCreationData()
		*/
		
		public function get releaseLineCreationData():Boolean
		{ return _releaseLineCreationData; }
		public function set releaseLineCreationData(val:Boolean):void
		{
			_releaseLineCreationData = val;
			_immutableClone = null;
		}
					
		/** Returns true if the ActionScript text engine was built with debugging code enabled. @private */
		static tlf_internal function get debugCodeEnabled():Boolean
		{
			CONFIG::debug   { return true; }
			CONFIG::release { return false; }
		}
		
		/** 
		* @copy IConfiguration#inlineGraphicResolverFunction
		* 
		* @playerversion Flash 10
		* @playerversion AIR 1.5
		* @langversion 3.0
		*
		* @see flashx.textLayout.elements.InlineGraphicElement InlineGraphicElement
		*/		
		public function get inlineGraphicResolverFunction():Function
		{ 
			return _inlineGraphicResolverFunction; 
		}
		public function set inlineGraphicResolverFunction(val:Function):void
		{
			_inlineGraphicResolverFunction = val;
			_immutableClone = null;
		}
	}
}
