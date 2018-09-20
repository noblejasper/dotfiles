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

package mx.rpc.remoting
{

import mx.core.mx_internal;
import mx.managers.CursorManager;
import mx.messaging.events.MessageEvent;
import mx.messaging.messages.AsyncMessage;
import mx.messaging.messages.IMessage;
import mx.messaging.messages.RemotingMessage;
import mx.resources.IResourceManager;
import mx.resources.ResourceManager;
import mx.rpc.AbstractOperation;
import mx.rpc.AbstractService;
import mx.rpc.AsyncDispatcher;
import mx.rpc.AsyncToken;
import mx.rpc.Fault;
import mx.rpc.events.FaultEvent;
import mx.rpc.mxml.Concurrency;
import mx.utils.ObjectUtil;

use namespace mx_internal;

/**
 * An Operation used specifically by RemoteObjects. An Operation is an individual method on a service.
 * An Operation can be called either by invoking the
 * function of the same name on the service or by accessing the Operation as a property on the service and
 * calling the <code>send()</code> method.
 *  
 *  @langversion 3.0
 *  @playerversion Flash 9
 *  @playerversion AIR 1.1
 *  @productversion Flex 3
 */
public class Operation extends AbstractOperation
{
    //---------------------------------
    // Constructor
    //---------------------------------

    /**
     * Creates a new Operation. This is usually done directly automatically by the RemoteObject
     * when an unknown operation has been accessed. It is not recommended that a developer use this constructor
     * directly.
     * 
     *  @param service The RemoteObject object defining the service.
     *
     *  @param name The name of the service.
     *  
     *  @langversion 3.0
     *  @playerversion Flash 9
     *  @playerversion AIR 1.1
     *  @productversion Flex 3
     */
    public function Operation(remoteObject:AbstractService = null, name:String = null)
    {
        super(remoteObject, name);

        argumentNames = [];

        this.remoteObject = mx.rpc.remoting.RemoteObject(remoteObject);
    }


    //---------------------------------
    // Properties
    //---------------------------------

    [Inspectable(enumeration="multiple,single,last", defaultValue="multiple", category="General")]
    /**
     * The concurrency for this Operation.  If it has not been explicitly set the setting from the RemoteObject
     * will be used.
     *  
     *  @langversion 3.0
     *  @playerversion Flash 9
     *  @playerversion AIR 1.1
     *  @productversion Flex 3
     */
    public function get concurrency():String
    {
        if (_concurrencySet)
        {
            return _concurrency;
        }
        //else
        return remoteObject.concurrency;
    }

    /**
     *  @private
     */
    public function set concurrency(c:String):void
    {
        _concurrency = c;
        _concurrencySet = true;
    }


    [Inspectable(defaultValue="true", category="General")]

    /**
     * When this value is true, anonymous objects returned are forced to bindable objects.
     *  
     *  @langversion 3.0
     *  @playerversion Flash 9
     *  @playerversion AIR 1.1
     *  @productversion Flex 3
     */
    override public function get makeObjectsBindable():Boolean
    {
        if (_makeObjectsBindableSet)
        {
            return _makeObjectsBindable;
        }

        return RemoteObject(service).makeObjectsBindable;    
    }

    override public function set makeObjectsBindable(b:Boolean):void
    {
        _makeObjectsBindable = b;
        _makeObjectsBindableSet = true;
    }

    /**
     * Whether this operation should show the busy cursor while it is executing.
     * If it has not been explicitly set the setting from the RemoteObject
     * will be used.
     *  
     *  @langversion 3.0
     *  @playerversion Flash 9
     *  @playerversion AIR 1.1
     *  @productversion Flex 3
     */
    public function get showBusyCursor():Boolean
    {
        if (_showBusyCursorSet)
        {
            return _showBusyCursor;
        }
        //else
        return remoteObject.showBusyCursor;
    }

    public function set showBusyCursor(sbc:Boolean):void
    {
        _showBusyCursor = sbc;
        _showBusyCursorSet = true;
    }


    /**
     * An ordered list of the names of the arguments to pass to a method invocation.  Since the arguments object is
     * a hashmap with no guaranteed ordering, this array helps put everything together correctly.
     * It will be set automatically by the MXML compiler, if necessary, when the Operation is used in tag form.
     *  
     *  @langversion 3.0
     *  @playerversion Flash 9
     *  @playerversion AIR 1.1
     *  @productversion Flex 3
     */
    public var argumentNames:Array;


    //--------------------------------------------------------------------------
    //
    // Private Variables
    // 
    //--------------------------------------------------------------------------

    private var _concurrency:String;
    
    private var _concurrencySet:Boolean;
    
    private var _makeObjectsBindableSet:Boolean;

    private var _showBusyCursor:Boolean;
    
    private var _showBusyCursorSet:Boolean;

    //---------------------------------
    // Methods
    //---------------------------------

    /**
     * @inheritDoc
     *  
     *  @langversion 3.0
     *  @playerversion Flash 9
     *  @playerversion AIR 1.1
     *  @productversion Flex 3
     */
    override public function send(... args:Array):AsyncToken
    {
        if (service != null)
            service.initialize();

        if (remoteObject.convertParametersHandler != null)
            args = remoteObject.convertParametersHandler(args);

        if (operationManager != null)
            return operationManager(args);

        if (Concurrency.SINGLE == concurrency && activeCalls.hasActiveCalls())
        {
            var token:AsyncToken = new AsyncToken(null);
            var m:String = resourceManager.getString(
                "rpc", "pendingCallExists");
            var fault:Fault = new Fault("ConcurrencyError", m);
            var faultEvent:FaultEvent = FaultEvent.createEvent(fault, token);
            new AsyncDispatcher(dispatchRpcEvent, [faultEvent], 10);
            return token;
        }

        // We delay endpoint initialization until now because MXML codegen may set 
        // the destination attribute after the endpoint and will clear out the 
        // channelSet.
        if (asyncRequest.channelSet == null && remoteObject.endpoint != null)
        {
            remoteObject.mx_internal::initEndpoint();
        }

        if (!args || (args.length == 0 && this.arguments))
        {
            if (this.arguments is Array)
            {
                args = this.arguments as Array;
            }
            else
            {
                args = [];
                for (var i:int = 0; i < argumentNames.length; ++i)
                {
                    args[i] = this.arguments[argumentNames[i]];
                }
            }
        }

        var message:RemotingMessage = new RemotingMessage();
        message.operation = name;
        message.body = args;
        message.source = RemoteObject(service).source;

        return invoke(message);
    }

    /**
     * @inheritDoc
     *  
     *  @langversion 3.0
     *  @playerversion Flash 9
     *  @playerversion AIR 1.1
     *  @productversion Flex 3
     */
    override public function cancel(id:String = null):AsyncToken
    {
        if (showBusyCursor)
        {
            CursorManager.removeBusyCursor();
        }
        return super.cancel(id);
    }

    override mx_internal function setService(ro:AbstractService):void
    {
        super.setService(ro);
        remoteObject = mx.rpc.remoting.RemoteObject(ro);
    }

    override mx_internal function invoke(message:IMessage, token:AsyncToken = null):AsyncToken
    {
        if (showBusyCursor)
        {
            CursorManager.setBusyCursor();
        }

        return super.invoke(message, token);
    }

    /*
     * Kill the busy cursor, find the matching call object and pass it back
     */
    override mx_internal function preHandle(event:MessageEvent):AsyncToken
    {
        if (showBusyCursor)
        {
            CursorManager.removeBusyCursor();
        }

        var wasLastCall:Boolean = activeCalls.wasLastCall(AsyncMessage(event.message).correlationId);
        var token:AsyncToken = super.preHandle(event);

        if (Concurrency.LAST == concurrency && !wasLastCall)
        {
            return null;
        }
        //else
        return token;
    }

    override mx_internal function processResult(message:IMessage, token:AsyncToken):Boolean
    {
        if (super.processResult(message, token))
        {
            if (remoteObject.convertResultHandler != null)
                _result = remoteObject.convertResultHandler(_result, this);
            return true;
        }
        return false;
    }

    //--------------------------------------------------------------------------
    //
    // Variables
    // 
    //--------------------------------------------------------------------------

    /**
     *  @private
     */
    private var resourceManager:IResourceManager = ResourceManager.getInstance();

    mx_internal var remoteObject:mx.rpc.remoting.RemoteObject;
}
    

}
