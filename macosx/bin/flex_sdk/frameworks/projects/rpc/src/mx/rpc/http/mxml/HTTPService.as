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

package mx.rpc.http.mxml
{

import flash.events.ErrorEvent;
import flash.events.ErrorEvent;

import mx.core.mx_internal;
import mx.core.IMXMLObject;
import mx.managers.CursorManager;
import mx.messaging.events.MessageEvent;
import mx.messaging.messages.IMessage;
import mx.messaging.messages.AsyncMessage;
import mx.resources.IResourceManager;
import mx.resources.ResourceManager;
import mx.rpc.AsyncToken;
import mx.rpc.AsyncDispatcher;
import mx.rpc.Fault;
import mx.rpc.http.HTTPService;
import mx.rpc.events.AbstractEvent;
import mx.rpc.events.FaultEvent;
import mx.rpc.mxml.Concurrency;
import mx.rpc.mxml.IMXMLSupport;
import mx.validators.Validator;

use namespace mx_internal;

[ResourceBundle("rpc")]

/**
 * You use the <code>&lt;mx:HTTPService&gt;</code> tag to represent an
 * HTTPService object in an MXML file. When you call the HTTPService object's
 * <code>send()</code> method, it makes an HTTP request to the
 * specified URL, and an HTTP response is returned. Optionally, you can pass
 * parameters to the specified URL. When you do not go through the server-based
 * proxy service, you can use only HTTP GET or POST methods. However, when you set
 * the useProxy  property to true and you use the server-based proxy service, you
 * can also use the HTTP HEAD, OPTIONS, TRACE, and DELETE methods.
 *
 * <p><b>Note:</b> Due to a software limitation, HTTPService does not generate
 * user-friendly error messages when using GET.
 * </p>
 *
 * @mxml
 * <p>
 * The &lt;mx:HTTPService&gt; tag accepts the following tag attributes:
 * </p>
 * <pre>
 * &lt;mx:HTTPService
 * <b>Properties</b>
 * concurrency="multiple|single|last"
 * contentType="application/x-www-form-urlencoded|application/xml"
 * destination="<i>DefaultHTTP</i>"
 * id="<i>No default.</i>"
 * method="GET|POST|HEAD|OPTIONS|PUT|TRACE|DELETE"
 * resultFormat="object|array|xml|e4x|flashvars|text"
 * showBusyCursor="false|true"
 * makeObjectsBindable="false|true"
 * url="<i>No default.</i>"
 * useProxy="false|true"
 * xmlEncode="<i>No default.</i>"
 * xmlDecode="<i>No default.</i>"
 *
 * <b>Events</b>
 * fault="<i>No default.</i>"
 * result="<i>No default.</i>"
 * /&gt;
 * </pre>
 *
 * The <code>&lt;mx:HTTPService&gt;</code> tag can have a single &lt;mx:request&gt; tag under which the parameters can be specified.
 * </p>
 *
 * @includeExample examples/HTTPServiceExample.mxml -noswf
 *
 * @see mx.rpc.http.HTTPService
 * @see mx.validators.Validator
 * @see mx.managers.CursorManager
 *  
 *  @langversion 3.0
 *  @playerversion Flash 9
 *  @playerversion AIR 1.1
 *  @productversion Flex 3
 */
public class HTTPService extends mx.rpc.http.HTTPService implements IMXMLSupport, IMXMLObject
{
    //--------------------------------------------------------------------------
    //
    // Constructor
    // 
    //--------------------------------------------------------------------------

    /**
     * Creates a new HTTPService. This constructor is usually called by the generated code of an MXML document.
     * You usually use the mx.rpc.http.HTTPService class to create an HTTPService in ActionScript.
     *
     * @param rootURL The URL the HTTPService should use when computing relative URLS.
     *
     * @param destination An HTTPService destination name in the service-config.xml file.
     *  
     *  @langversion 3.0
     *  @playerversion Flash 9
     *  @playerversion AIR 1.1
     *  @productversion Flex 3
     */
    public function HTTPService(rootURL:String = null, destination:String = null)
    {
        super(rootURL, destination);

    }

    //--------------------------------------------------------------------------
    //
    // Variables
    // 
    //--------------------------------------------------------------------------

    /**
     *  @private
     */
    private var resourceManager:IResourceManager =
									ResourceManager.getInstance();

    /**
     * Called after the implementing object has been created and all
     * component properties specified on the MXML tag have been
     * initialized. 
     *
     * If you create this class in ActionScript and want it to function with validation, you must
     * call this method and pass in the MXML document and the
     * HTTPService's <code>id</code>.
     *
     * @param document The MXML document that created this object.
     *
     * @param id The identifier used by <code>document</code> to refer
     * to this object. If the object is a deep property on document,
     * <code>id</code> is null. 
     *  
     *  @langversion 3.0
     *  @playerversion Flash 9
     *  @playerversion AIR 1.1
     *  @productversion Flex 3
     */
    public function initialized(document:Object, id:String):void
    {
        this.id = id;
        this.document = document;
    }


    //--------------------------------------------------------------------------
    //
    // Internal Methods
    // 
    //--------------------------------------------------------------------------

    /**
     * If this event is a fault, and the event type does not
     * have a listener, we notify the parent document.  If the
     * parent document does not have a listener, then we throw
     * a runtime exception.  However, this is an asynchronous runtime
     * exception which is only exposed through the debug player.
     * A listener should be defined.
     *
     * @private
     */
    override mx_internal function dispatchRpcEvent(event:AbstractEvent):void
    {
        event.callTokenResponders();
        if (!event.isDefaultPrevented())
        {
            if (hasEventListener(event.type))
            {
                dispatchEvent(event);
            }
            else if (event is FaultEvent && (event.token == null || !event.token.hasResponder()))
            {
                if (document && document.willTrigger(ErrorEvent.ERROR))
                {
                    var evt:ErrorEvent = new ErrorEvent(ErrorEvent.ERROR, true, true);
                    evt.text = FaultEvent(event).fault.faultString;
                    document.dispatchEvent(evt);
                }
                else
                {
               	  //need to monitor right before throwing, otherwise monitor should be last
			    monitorRpcEvent(event); 
                    
                    // last-ditch effort to notify the user that something went wrong
                    throw FaultEvent(event).fault;
                }
            }
            monitorRpcEvent(event);

        }
        
    }


    //--------------------------------------------------------------------------
    //
    // Private Variables
    // 
    //--------------------------------------------------------------------------

	private var document:Object; //keep the document for validation
    
	private var id:String; //need to know our own id for validation
    
}

}
