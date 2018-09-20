////////////////////////////////////////////////////////////////////////////////
//
//  ADOBE SYSTEMS INCORPORATED
//  Copyright 2005-2007 Adobe Systems Incorporated
//  All Rights Reserved.
//
//  NOTICE: Adobe permits you to use, modify, and distribute this file
//  in accordance with the terms of the license agreement accompanying it.
//
////////////////////////////////////////////////////////////////////////////////

package mx.core
{

import flash.display.LoaderInfo;
import flash.display.MovieClip;
import flash.events.ErrorEvent;
import flash.events.Event;
import flash.events.IEventDispatcher;
import flash.events.IOErrorEvent;
import flash.events.SecurityErrorEvent;
import flash.events.TimerEvent;
import flash.system.ApplicationDomain;
import flash.text.TextField;
import flash.text.TextFieldAutoSize;
import flash.utils.Dictionary;
import flash.utils.Timer;
import flash.utils.getDefinitionByName;

import mx.core.RSLItem;
import mx.core.RSLListLoader;
import mx.events.ModuleEvent;
import mx.events.Request;
import mx.events.RSLEvent;
import mx.managers.SystemManagerGlobals;
import mx.resources.IResourceManager;
import mx.resources.ResourceManager;
import mx.utils.LoaderUtil;

use namespace mx_internal;

[ExcludeClass]

/**
 *  @private
 */
public class FlexModuleFactory extends MovieClip
	implements IFlexModuleFactory
{
	include "../core/Version.as";

	//--------------------------------------------------------------------------
	//
	//  Class constants
	//
	//--------------------------------------------------------------------------

    /**
	 *  @private
	 */
    private static const INIT_STATE:int = 0;
    
    /**
	 *  @private
	 */
	private static const RSL_START_LOAD_STATE:int = 1;
    
    /**
	 *  @private
	 */
	private static const APP_LOAD_STATE:int = 2;
    
    /**
	 *  @private
	 */
	private static const APP_START_STATE:int = 3;
    
    /**
	 *  @private
	 */
	private static const APP_RUNNING_STATE:int = 4;
    
    /**
	 *  @private
	 */
	private static const ERROR_STATE:int = 5;

    /**
	 *  @private
	 */
	private static const RSL_LOADING_STATE:int = 6;
    

	//--------------------------------------------------------------------------
	//
	//  Constructor
	//
	//--------------------------------------------------------------------------

    /**
	 *  @private
	 */
	public function FlexModuleFactory()
    {
		super();

		mixinList = info()["mixins"];

		stop(); // Make sure to stop the playhead on the currentframe
        
        loaderInfo.addEventListener(Event.INIT, moduleInitHandler);
		loaderInfo.addEventListener(Event.COMPLETE, moduleCompleteHandler);

	    var docFrame:int = totalFrames == 1 ? 0 : 1;

        addEventListener(Event.ENTER_FRAME, docFrameListener);

		timer = new Timer(100);
		timer.addEventListener(TimerEvent.TIMER, timerHandler);
		timer.start();
    }

    private function docFrameListener(event:Event):void
    {
        if (currentFrame == 2)
        {
            removeEventListener(Event.ENTER_FRAME, docFrameListener);
            if (totalFrames > 2)
                addEventListener(Event.ENTER_FRAME, extraFrameListener);
            
            docFrameHandler();
        }
    }
    
    private function extraFrameListener(event:Event):void
    {
        if (lastFrame == currentFrame)
            return;
        
        lastFrame = currentFrame;
        
        if (currentFrame + 1 > totalFrames)
            removeEventListener(Event.ENTER_FRAME, extraFrameListener);
        
        extraFrameHandler();
    }

    //--------------------------------------------------------------------------
	//
	//  Variables
	//
	//--------------------------------------------------------------------------

  	/**
	 *  @private
	 */
	private var rslListLoader:RSLListLoader;
	
    /**
	 *  @private
	 */
	private var mixinList:Array;

    /**
	 *  @private
	 */
    private var state:int = INIT_STATE;
  
    /**
	 *  @private
	 */
	private var appReady:Boolean = false;
    
    /**
     *  @private
     */
    private var appInitDone:Boolean = false;
    
    /**
	 *  @private
	 */
	private var appLoaded:Boolean = false;
    
    /**
	 *  @private
	 */
	private var timer:Timer = null;
    
    /**
     *  @private
     *  Track which frame was last processed
     */
    private var lastFrame:int;
    
    /**
	 *  @private
	 */
	private var nextFrameTimer:Timer = null;
    
    /**
	 *  @private
	 */
	private var errorMessage:String = null;

    /**
     *  @private
     * 
     *  This exists only to provide a reference to this 
     *  module's resource bundles. This module is opting
     *  into referencing its own resource bundles so the
     *  ResourceManager does not need to do it. This 
     *  arrangement keeps the ResourceManager from pinning
     *  the module in memory.
     */
    private var resourceBundles:Array;
    
    /**
     *  @private
     *  Array of RSLData objects that represent the list of RSLs this
     *  module factory is loading. Each element of the Array is an 
     *  Array of RSLData.
     */ 
    private var rslDataList:Array
    
	//--------------------------------------------------------------------------
	//
	//  Properties: IFlexModuleFactory
	//
	//--------------------------------------------------------------------------

    //----------------------------------
    //  allowDomainsInNewRSLs
    //----------------------------------
    
    /**
     *  @private
     */ 
    private var _allowDomainsInNewRSLs:Boolean = true;
    
    /**
     *  @inheritDoc
     *
     *  @langversion 3.0
     *  @playerversion Flash 10.2
     *  @playerversion AIR 2.6
     *  @productversion Flex 4.5
     */   
    public function get allowDomainsInNewRSLs():Boolean
    {
        return _allowDomainsInNewRSLs;
    }
    
    /**
     *  @private
     */ 
    public function set allowDomainsInNewRSLs(value:Boolean):void
    {
        _allowDomainsInNewRSLs = value;
    }
    
    //----------------------------------
    //  allowInsecureDomainsInNewRSLs
    //----------------------------------
    
    /**
     *  @private
     */ 
    private var _allowInsecureDomainsInNewRSLs:Boolean = true;
    
    /**
     *  @inheritDoc
     *
     *  @langversion 3.0
     *  @playerversion Flash 10.2
     *  @playerversion AIR 2.6
     *  @productversion Flex 4.5
     */   
    public function get allowInsecureDomainsInNewRSLs():Boolean
    {
        return _allowInsecureDomainsInNewRSLs;
    }
    
    /**
     *  @private
     */ 
    public function set allowInsecureDomainsInNewRSLs(value:Boolean):void
    {
        _allowInsecureDomainsInNewRSLs = value;
    }
    
    //----------------------------------
    //  preloadedRSLs
    //----------------------------------
    
    /**
     *  @inheritDoc 
     *  
     */
    public function  get preloadedRSLs():Dictionary
    {
        // Overridden by compiler generate code.
        return null;                
    }
    
    //--------------------------------------------------------------------------
    //
    //  Methods: IFlexModuleFactory
    //
    //--------------------------------------------------------------------------
    
    /**
     *  @inheritDoc 
     *  
     *  @langversion 3.0
     *  @playerversion Flash 9
     *  @playerversion AIR 1.1
     *  @productversion Flex 4.5
     */ 
    public function addPreloadedRSL(loaderInfo:LoaderInfo, rsl:Vector.<RSLData>):void
    {
        preloadedRSLs[loaderInfo] = rsl;
        if (hasEventListener(RSLEvent.RSL_ADD_PRELOADED))
        {
            var rslEvent:RSLEvent = new RSLEvent(RSLEvent.RSL_ADD_PRELOADED);
            rslEvent.loaderInfo = loaderInfo;
            dispatchEvent(rslEvent);
        }
        
    }
    
    /**
   	 *  @private
	 *  This method is overridden in the autogenerated subclass.
	 *  It is part of TLF's ISWFContext interface.
	 *  Although this class does not declare that it implements this interface,
	 *  the autogenerated subclass does.
   	 */
    public function callInContext(fn:Function, thisArg:Object,
								  argArray:Array, returns:Boolean = true):*
    {
        return undefined;
    }

    /**
   	 *  @private
	 *  This method is overridden in the autogenerated subclass.
   	 */
    public function create(... params):Object
    {
	    var mainClassName:String = info()["mainClassName"];
	    
		if (mainClassName == null)
	    {
            var url:String = loaderInfo.loaderURL;
            var dot:Number = url.lastIndexOf(".");
            var slash:Number = url.lastIndexOf("/");
            mainClassName = url.substring(slash + 1, dot);
	    }
	   
        var mainClass:Class = Class(getDefinitionByName(mainClassName));

        return mainClass? new mainClass() : null;
    }

  	/**
   	 *  @private
   	 */
    public function info():Object
    {
        return {};
    }

    /**
     *  Calls Security.allowDomain() for the SWF associated with this FlexModuleFactory.
     *  plus all the SWFs assocatiated with RSLs preloaded by this FlexModuleFactory.
     * 
     */  
    public function allowDomain(... domains):void
    {
        // Overridden by compiler generated code.
    }
    
    /**
     *  Calls Security.allowInsecureDomain() for the SWF associated with this FlexModuleFactory
     *  plus all the SWFs assocatiated with RSLs preloaded by this FlexModuleFactory.
     * 
     */  
    public function allowInsecureDomain(... domains):void
    {
        // Overridden by compiler generated code.
    }
    
    /**
     * @private
     *  A map of fully-qualified interface names,
     *  such as "mx.managers::IPopUpManager",
     *  to instances,
     */
    private var implMap:Object = {};
    
    /**
     * @private
     *  Adds an interface-name-to-implementation-class mapping to the registry,
     *  if a class hasn't already been registered for the specified interface.
     *  The class must implement a getInstance() method which returns
     *  its singleton instance.
     */
    public function registerImplementation(interfaceName:String,
                                           impl:Object):void
    {
        var c:Object = implMap[interfaceName];
        if (!c)
            implMap[interfaceName] = impl;
    }
    
    /**
     * @private
     *  Returns the singleton instance of the implementation class
     *  that was registered for the specified interface,
     *  by looking up the class in the registry
     *  and calling its getInstance() method.
     * 
     *  This method should not be called at static initialization time,
     *  because the factory class may not have called registerImplementation() yet.
     */
    public function getImplementation(interfaceName:String):Object
    {
        var c:Object = implMap[interfaceName];
        return c;
    }
    
    //--------------------------------------------------------------------------
	//
	//  Methods
	//
	//--------------------------------------------------------------------------

   /**
    *  @inheritDoc
    *  
    *  @langversion 3.0
    *  @playerversion Flash 9
    *  @playerversion AIR 1.1
    *  @productversion Flex 3
    */
    public function getDefinitionByName(name:String):Object
    {
       const domain:ApplicationDomain =
		info()["currentDomain"] as ApplicationDomain; 

       var definition:Object;
       if (domain.hasDefinition(name))
          definition = domain.getDefinition(name);

       return definition;
    }

    /**
	 *  @private
	 */
    private function update():void
    {
        switch (state)
        {
            case INIT_STATE:
			{
                if (!appInitDone)
                    return;
                
                getRSLInfo();
                if (rslListLoader.isDone())
                    state = APP_LOAD_STATE;
                else
                    state = RSL_START_LOAD_STATE;
				break;
			}

            case RSL_START_LOAD_STATE:
			{
			    // start loading all the rsls
                rslListLoader.load(null,
                				   rslCompleteHandler,
                				   rslErrorHandler,
                				   rslErrorHandler,
                				   rslErrorHandler);
                state = RSL_LOADING_STATE;
                break;
            }
            case RSL_LOADING_STATE:
            {
                if (rslListLoader.isDone())
                {
                    rslListLoader = null;
                    state = APP_LOAD_STATE;
                }
                break;
			}

            case APP_LOAD_STATE:
			{
                if (appLoaded)
                {
                    deferredNextFrame();
                    state = APP_START_STATE;
                }
                break;
			}

            case APP_START_STATE:
			{
                if (appReady)
                {
            		if (mixinList && mixinList.length > 0)
            		{
            		    var n:int = mixinList.length;
            			for (var i:int = 0; i < n; i++)
            		    {
            		        var c:Class;
            		        try
            		        {
            		           c =	Class(getDefinitionByName(mixinList[i]));
            		           c["init"](this);
            		        }
            		        catch(e:Error)
            		        {
							}
            		    }
                    }

                    state = APP_RUNNING_STATE;
        			timer.removeEventListener(TimerEvent.TIMER, timerHandler);
        			// Stop the timer.
        			timer.reset();

                    dispatchEvent(new Event("ready"));
                    
                    loaderInfo.removeEventListener(Event.COMPLETE, moduleCompleteHandler);
                }
                break;
			}

            case ERROR_STATE:
			{
                if (timer != null)
                {
        			timer.removeEventListener(TimerEvent.TIMER, timerHandler);
        			// stop the timer
        			timer.reset();
                }

                var tf:TextField = new TextField();
                tf.text = errorMessage;
                tf.x = 0;
                tf.y = 0;
                tf.autoSize = TextFieldAutoSize.LEFT;
                addChild(tf);
                
                dispatchEvent(new ModuleEvent(ModuleEvent.ERROR, false, false, 
                              0, 0, errorMessage));
                
                loaderInfo.removeEventListener(Event.COMPLETE, moduleCompleteHandler);
                break;
			}
        }
    }

    /**
     *  @private
     *  Get the RSL URLs and add them to the RSLListLoader.
     */   
    private function getRSLInfo():void
    {
        var rsls:Array = info()["rsls"];
        var cdRsls:Array = info()["cdRsls"];
        
        // Put cross-domain RSL information in the RSL list.
        var rslItemList:Array = [];
        var n:int;
        var i:int;
        if (cdRsls && cdRsls.length > 0)
        {
            rslDataList = LoaderUtil.processRequiredRSLs(this, cdRsls);
            
            var normalizedURL:String = LoaderUtil.normalizeURL(this.loaderInfo);
            var crossDomainRSLItem:Class = Class(getDefinitionByName("mx.core::CrossDomainRSLItem"));
            n = rslDataList.length;
            for (i = 0; i < n; i++)
            {
                var rslWithFailovers:Array = rslDataList[i];
                
                // If crossDomainRSLItem is null, then this is a compiler error. It should not be null.
                var cdNode:Object = new crossDomainRSLItem(rslWithFailovers,
                    normalizedURL,
                    this);
                rslItemList.push(cdNode);               
            }
        }
        
        // Append RSL information in the RSL list.
        if (rsls != null && rsls.length > 0)
        {
            if (normalizedURL == null)
                normalizedURL = LoaderUtil.normalizeURL(this.loaderInfo);
            
            n = rsls.length;
            for (i = 0; i < n; i++)
            {
                var node:RSLItem = new RSLItem(rsls[i].url, 
                                               normalizedURL,
                                               this);
                rslItemList.push(node);
            }
        }
        
        rslListLoader = new RSLListLoader(rslItemList);
    }
    
    /**
	 *  @private
	 */
    public function autorun():Boolean
    {
        return true;
    }

    /**
	 *  @private
	 */
    private function displayError(msg:String):void
    {
        errorMessage = msg;
        state = ERROR_STATE;
        update();
    }
	
    /**
	 *  @private
	 */
	private function docFrameHandler(event:Event = null):void
	{
		// Register singleton classes.
		// Note: getDefinitionByName() will return null
		// if the class can't be found.

		Singleton.registerClass("mx.managers::IBrowserManager",
			Class(getDefinitionByName("mx.managers::BrowserManagerImpl")));

        Singleton.registerClass("mx.managers::ICursorManager",
			Class(getDefinitionByName("mx.managers::CursorManagerImpl")));

        Singleton.registerClass("mx.managers::IDragManager",
			Class(getDefinitionByName("mx.managers::DragManagerImpl")));

        Singleton.registerClass("mx.managers::IHistoryManager",
			Class(getDefinitionByName("mx.managers::HistoryManagerImpl")));

        Singleton.registerClass("mx.managers::ILayoutManager",
			Class(getDefinitionByName("mx.managers::LayoutManager")));

        Singleton.registerClass("mx.managers::IPopUpManager",
			Class(getDefinitionByName("mx.managers::PopUpManagerImpl")));

		Singleton.registerClass("mx.resources::IResourceManager",
			Class(getDefinitionByName("mx.resources::ResourceManagerImpl")));

        Singleton.registerClass("mx.styles::IStyleManager",
			Class(getDefinitionByName("mx.styles::StyleManagerImpl")));

        Singleton.registerClass("mx.styles::IStyleManager2",
			Class(getDefinitionByName("mx.styles::StyleManagerImpl")));

        Singleton.registerClass("mx.managers::IToolTipManager2",
			Class(getDefinitionByName("mx.managers::ToolTipManagerImpl")));

		appReady = true;
        
        // The resources must be installed before update() creates components
		// (such as DateChooswer) that might need them immediately.
		installCompiledResourceBundles();
		
		update();
        
		if (currentFrame < totalFrames)
            deferredNextFrame();
    }
    
    /**
	 *  @private
	 */
	private function installCompiledResourceBundles():void
	{
		//trace("FlexModuleFactory.installCompiledResourceBundles");

		var info:Object = this.info();
		
		var applicationDomain:ApplicationDomain =
            info["currentDomain"];

		var compiledLocales:Array /* of String */ =
			info["compiledLocales"];

		var compiledResourceBundleNames:Array /* of String */ =
			info["compiledResourceBundleNames"];
		
		var resourceManager:IResourceManager =
			ResourceManager.getInstance();
		
		resourceBundles = resourceManager.installCompiledResourceBundles(
			applicationDomain, compiledLocales, compiledResourceBundleNames, true);

		// If the localeChain wasn't specified in the FlashVars of the SWF's
		// HTML wrapper, or in the query parameters of the SWF URL,
		// then initialize it to the list of compiled locales,
        // sorted according to the system's preferred locales as reported by
        // Capabilities.languages or Capabilities.language.
		// For example, if the applications was compiled with, say,
		// -locale=en_US,ja_JP and Capabilities.languages reports [ "ja-JP" ],
        // set the localeChain to [ "ja_JP" "en_US" ].
		if (!resourceManager.localeChain)
			resourceManager.initializeLocaleChain(compiledLocales);
	}

    /**
	 *  @private
	 */
    private function deferredNextFrame():void
    {
        if (currentFrame + 1 <= framesLoaded)
		{
            nextFrame();
		}
        else
        {
            // Next frame isn't baked yet, we'll check back...
    		nextFrameTimer = new Timer(100);
		    nextFrameTimer.addEventListener(TimerEvent.TIMER,
											nextFrameTimerHandler);
		    nextFrameTimer.start();
        }
    }
    /**
	 *  @private
	 */
	private function extraFrameHandler(event:Event = null):void
	{
	    var frameList:Object = info()["frames"];

	    if (frameList && frameList[currentLabel])
	    {
	        var c:Class;
	        try
	        {
	            c = Class(getDefinitionByName(frameList[currentLabel]));
	            c["frame"](this);
	        }
	        catch(e:Error)
	        {
			}
	    }

	    if (currentFrame < totalFrames)
            deferredNextFrame();
	}

	//--------------------------------------------------------------------------
	//
	//  Event handlers
	//
	//--------------------------------------------------------------------------

    /**
	 *  @private
	 */
    private function rslCompleteHandler(event:Event):void
    {
        if (event.target is LoaderInfo)
        {
            var rslIndex:int = rslListLoader.getIndex();
            var rsl:Vector.<RSLData> = Vector.<RSLData>(rslDataList[rslIndex]);
            var moduleFactory:IFlexModuleFactory = this;
            if (rsl && rsl[0].moduleFactory)
                moduleFactory = rsl[0].moduleFactory; 
            
            if (moduleFactory == this)
                preloadedRSLs[event.target] =  rsl;
            else 
                moduleFactory.addPreloadedRSL(LoaderInfo(event.target), rsl);
        }

        update();
    }

    /**
	 *  @private
	 */
    private function rslErrorHandler(event:Event):void
    {
        var rsl:RSLItem = rslListLoader.getItem(rslListLoader.getIndex());
        var detailedError:String;
        var message:String;
        
        if (event is ErrorEvent)
            detailedError = ErrorEvent(event).text;
        
        if (!detailedError)
            detailedError = "";
            
        message = "RSL " + rsl.urlRequest.url + " failed to load. " + detailedError;              
        displayError(message);
    }

    /**
     *  @private
     */
    private function moduleInitHandler(event:Event):void
    {
        loaderInfo.removeEventListener(Event.INIT, moduleInitHandler);
        appInitDone = true;
        update();
    }

    /**
	 *  @private
	 */
    private function moduleCompleteHandler(event:Event):void
    {
        appLoaded = true;
		update();
    }

    /**
	 *  @private
	 */
	private function timerHandler(event:TimerEvent):void
	{
	    if (totalFrames > 2 && framesLoaded >= 2 || 
			framesLoaded == totalFrames)
		{
            appLoaded = true;
		}
		
		update();
    }

    /**
	 *  @private
	 */
	private function nextFrameTimerHandler(event:TimerEvent):void
	{
	    if (currentFrame + 1 <= framesLoaded)
	    {
	        nextFrame();
            nextFrameTimer.removeEventListener(TimerEvent.TIMER,
											   nextFrameTimerHandler);
        	// stop the timer
        	nextFrameTimer.reset();
        }
    }
}

}
