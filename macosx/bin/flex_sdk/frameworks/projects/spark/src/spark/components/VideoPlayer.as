////////////////////////////////////////////////////////////////////////////////
//
//  ADOBE SYSTEMS INCORPORATED
//  Copyright 2009 Adobe Systems Incorporated
//  All Rights Reserved.
//
//  NOTICE: Adobe permits you to use, modify, and distribute this file
//  in accordance with the terms of the license agreement accompanying it.
//
////////////////////////////////////////////////////////////////////////////////

package spark.components
{

import flash.display.DisplayObject;
import flash.display.StageDisplayState;
import flash.events.Event;
import flash.events.FullScreenEvent;
import flash.events.KeyboardEvent;
import flash.events.MouseEvent;
import flash.events.TimerEvent;
import flash.geom.Rectangle;
import flash.media.Video;
import flash.system.ApplicationDomain;
import flash.utils.Timer;

import mx.core.FlexGlobals;
import mx.core.IVisualElementContainer;
import mx.core.mx_internal;
import mx.events.FlexEvent;
import mx.managers.PopUpManager;
import mx.utils.BitFlagUtil;

import org.osmf.events.LoadEvent;
import org.osmf.events.MediaPlayerStateChangeEvent;
import org.osmf.events.TimeEvent;
import org.osmf.media.MediaPlayerState;

import spark.components.mediaClasses.MuteButton;
import spark.components.mediaClasses.ScrubBar;
import spark.components.mediaClasses.VolumeBar;
import spark.components.supportClasses.ButtonBase;
import spark.components.supportClasses.SkinnableComponent;
import spark.components.supportClasses.ToggleButtonBase;
import spark.core.IDisplayText;
import spark.events.TrackBaseEvent;

use namespace mx_internal;

//--------------------------------------
//  Events
//--------------------------------------

/**
 *  Dispatched when the data is received as a download operation progresses.
 *  This event is only dispatched when playing a video by downloading it 
 *  directly from a server, typically by issuing an HTTP request.
 *  It is not displatched when playing a video from a special media server, 
 *  such as Flash Media Server.
 * 
 *  <p>This event may not be dispatched when the source is set to null or a playback
 *  error occurs.</p>
 *
 *  @eventType org.osmf.events.LoadEvent.BYTES_LOADED_CHANGE
 *  
 *  @langversion 3.0
 *  @playerversion Flash 10
 *  @playerversion AIR 1.0
 *  @productversion Flex 4
 */
[Event(name="bytesLoadedChange",type="org.osmf.events.LoadEvent")]

/**
 *  Dispatched when the playhead reaches the duration for playable media.
 * 
 *  @eventType org.osmf.events.TimeEvent.COMPLETE
 *  
 *  @langversion 3.0
 *  @playerversion Flash 10
 *  @playerversion AIR 1.0
 *  @productversion Flex 4
 */  
[Event(name="complete", type="org.osmf.events.TimeEvent")]

/**
 *  Dispatched when the <code>currentTime</code> property of the MediaPlayer has changed.
 * 
 *  <p>This event may not be dispatched when the source is set to null or a playback
 *  error occurs.</p>
 *
 *  @eventType org.osmf.events.TimeEvent.CURRENT_TIME_CHANGE
 *
 *  @langversion 3.0
 *  @playerversion Flash 10
 *  @playerversion AIR 1.0
 *  @productversion Flex 4
 */
[Event(name="currentTimeChange",type="org.osmf.events.TimeEvent")]

/**
 *  Dispatched when the <code>duration</code> property of the media has changed.
 * 
 *  <p>This event may not be dispatched when the source is set to null or a playback
 *  error occurs.</p>
 * 
 *  @eventType org.osmf.events.TimeEvent.DURATION_CHANGE
 * 
 *  @langversion 3.0
 *  @playerversion Flash 10
 *  @playerversion AIR 1.0
 *  @productversion Flex 4
 */
[Event(name="durationChange", type="org.osmf.events.TimeEvent")]

/**
 *  Dispatched when the MediaPlayer's state has changed.
 * 
 *  @eventType org.osmf.events.MediaPlayerStateChangeEvent.MEDIA_PLAYER_STATE_CHANGE
 *  
 *  @langversion 3.0
 *  @playerversion Flash 10
 *  @playerversion AIR 1.0
 *  @productversion Flex 4
 */ 
[Event(name="mediaPlayerStateChange", type="org.osmf.events.MediaPlayerStateChangeEvent")]

//--------------------------------------
//  Styles
//--------------------------------------

include "../styles/metadata/BasicInheritingTextStyles.as";

/**
 *  Controls the visibility of the drop shadow for this component.
 *
 *  @default true
 * 
 *  @langversion 3.0
 *  @playerversion Flash 10
 *  @playerversion AIR 1.5
 *  @productversion Flex 4
 */
[Style(name="dropShadowVisible", type="Boolean", inherit="no", theme="spark")]

/**
 *  The time, in milli-seconds, to wait in fullscreen mode with no user-interaction 
 *  before hiding the video playback controls.  
 * 
 *  <p>If set to <code>Infinity</code>, then the playback controls will not 
 *  be hidden in fullscreen mode.  Changing this value while already in 
 *  fullscreen mode has no effect.</p>
 *  
 *  @default 3000
 * 
 *  @langversion 3.0
 *  @playerversion Flash 10
 *  @playerversion AIR 1.5
 *  @productversion Flex 4
 */
[Style(name="fullScreenHideControlsDelay", type="Number", format="Time", inherit="no")]

/**
 *  @copy spark.components.supportClasses.GroupBase#style:symbolColor
 *  
 *  @langversion 3.0
 *  @playerversion Flash 10
 *  @playerversion AIR 1.5
 *  @productversion Flex 4
 */ 
[Style(name="symbolColor", type="uint", format="Color", inherit="yes", theme="spark")]

//--------------------------------------
//  SkinStates
//--------------------------------------

/**
 *  Uninitialized state of the VideoPlayer.  
 *  The Video Player has been constructed at this point, 
 *  but the source has not been set and no connection 
 *  attempt is in progress.
 *  
 *  @langversion 3.0
 *  @playerversion Flash 10
 *  @playerversion AIR 1.5
 *  @productversion Flex 4
 */
[SkinState("uninitialized")]

/**
 *  Loading state of the VideoPlayer.
 *  The VideoPlayer is loading or connecting to the source. 
 *  
 *  @langversion 3.0
 *  @playerversion Flash 10
 *  @playerversion AIR 1.5
 *  @productversion Flex 4
 */
[SkinState("loading")]

/**
 *  Ready state of the VideoPlayer.
 *  The video is ready to be played.
 *  
 *  @langversion 3.0
 *  @playerversion Flash 10
 *  @playerversion AIR 1.5
 *  @productversion Flex 4
 */
[SkinState("ready")]

/**
 *  Playing state of the VideoPlayer
 *  
 *  @langversion 3.0
 *  @playerversion Flash 10
 *  @playerversion AIR 1.5
 *  @productversion Flex 4
 */
[SkinState("playing")]

/**
 *  Paused state of the VideoPlayer
 *  
 *  @langversion 3.0
 *  @playerversion Flash 10
 *  @playerversion AIR 1.5
 *  @productversion Flex 4
 */
[SkinState("paused")]

/**
 *  Buffering state of the VideoPlayer
 *  
 *  @langversion 3.0
 *  @playerversion Flash 10
 *  @playerversion AIR 1.5
 *  @productversion Flex 4
 */
[SkinState("buffering")]

/**
 *  Playback Error state of the VideoPlayer. 
 *  An error was encountered while trying to play the video.
 *  
 *  @langversion 3.0
 *  @playerversion Flash 10
 *  @playerversion AIR 1.5
 *  @productversion Flex 4
 */
[SkinState("playbackError")]

/**
 *  Disabled state of the VideoPlayer
 *  
 *  @langversion 3.0
 *  @playerversion Flash 10
 *  @playerversion AIR 1.5
 *  @productversion Flex 4
 */
[SkinState("disabled")]

/**
 *  Uninitialized state of the VideoPlayer when 
 *  in full screen mode.
 *  The Video Player has been constructed at this point, 
 *  but the source has not been set and no connection 
 *  attempt is in progress.
 *  
 *  @langversion 3.0
 *  @playerversion Flash 10
 *  @playerversion AIR 1.5
 *  @productversion Flex 4
 */
[SkinState("uninitializedAndFullScreen")]

/**
 *  Loading state of the VideoPlayer when 
 *  in full screen mode.
 *  The VideoPlayer is loading or connecting to the source. 
 *  
 *  @langversion 3.0
 *  @playerversion Flash 10
 *  @playerversion AIR 1.5
 *  @productversion Flex 4
 */
[SkinState("loadingAndFullScreen")]

/**
 *  Ready state of the VideoPlayer when 
 *  in full screen mode.  The video is ready to be played.
 *  
 *  @langversion 3.0
 *  @playerversion Flash 10
 *  @playerversion AIR 1.5
 *  @productversion Flex 4
 */
[SkinState("readyAndFullScreen")]

/**
 *  Playing state of the VideoPlayer when 
 *  in full screen mode.
 *  
 *  @langversion 3.0
 *  @playerversion Flash 10
 *  @playerversion AIR 1.5
 *  @productversion Flex 4
 */
[SkinState("playingAndFullScreen")]

/**
 *  Paused state of the VideoPlayer when 
 *  in full screen mode.
 *  
 *  @langversion 3.0
 *  @playerversion Flash 10
 *  @playerversion AIR 1.5
 *  @productversion Flex 4
 */
[SkinState("pausedAndFullScreen")]

/**
 *  Buffering state of the VideoPlayer when 
 *  in full screen mode.
 *  
 *  @langversion 3.0
 *  @playerversion Flash 10
 *  @playerversion AIR 1.5
 *  @productversion Flex 4
 */
[SkinState("bufferingAndFullScreen")]

/**
 *  Playback Error state of the VideoPlayer when 
 *  in full screen mode.  
 *  An error was encountered while trying to play the video.
 *  
 *  @langversion 3.0
 *  @playerversion Flash 10
 *  @playerversion AIR 1.5
 *  @productversion Flex 4
 */
[SkinState("playbackErrorAndFullScreen")]

/**
 *  Disabled state of the VideoPlayer when 
 *  in full screen mode.
 *  
 *  @langversion 3.0
 *  @playerversion Flash 10
 *  @playerversion AIR 1.5
 *  @productversion Flex 4
 */
[SkinState("disabledAndFullScreen")]

//--------------------------------------
//  Excluded APIs
//--------------------------------------

[Exclude(name="focusBlendMode", kind="style")]
[Exclude(name="focusThickness", kind="style")]

//--------------------------------------
//  Other metadata
//--------------------------------------

[AccessibilityClass(implementation="spark.accessibility.VideoPlayerAccImpl")]

[DefaultProperty("source")]

[IconFile("VideoPlayer.png")]

/**
 * Because this component does not define a skin for the mobile theme, Adobe
 * recommends that you not use it in a mobile application. Alternatively, you
 * can define your own mobile skin for the component. For more information,
 * see <a href="http://help.adobe.com/en_US/flex/mobileapps/WS19f279b149e7481c698e85712b3011fe73-8000.html">Basics of mobile skinning</a>.
 */
[DiscouragedForProfile("mobileDevice")]

/**
 *  The VideoPlayer control is a skinnable video player that supports
 *  progressive download, multi-bitrate streaming, and streaming video.
 *  It supports playback of FLV and F4v files. The VideoPlayer control
 *  contains a full-featured UI for controlling video playback.
 * 
 *  <p><code>VideoDisplay</code> is the chromeless version that does not support skinning.
 *  It is useful when you do not want the user to interact with the control.</p>
 *
 *  <p>The VideoPlayer control has the following default characteristics:</p>
 *     <table class="innertable">
 *        <tr>
 *           <th>Characteristic</th>
 *           <th>Description</th>
 *        </tr>
 *        <tr>
 *           <td>Default size</td>
 *           <td>263 pixels wide by 184 pixels high</td>
 *        </tr>
 *        <tr>
 *           <td>Minimum size</td>
 *           <td>0</td>
 *        </tr>
 *        <tr>
 *           <td>Maximum size</td>
 *           <td>10000 pixels wide and 10000 pixels high</td>
 *        </tr>
 *        <tr>
 *           <td>Default skin class</td>
 *           <td>spark.skins.spark.VideoPlayerSkin</td>
 *        </tr>
 *     </table>
 *
 *  @see spark.components.VideoDisplay
 *  @see spark.skins.spark.VideoPlayerSkin
 *  @see spark.skins.spark.mediaClasses.fullScreen.FullScreenButtonSkin
 *  @see spark.skins.spark.mediaClasses.fullScreen.MuteButtonSkin
 *  @see spark.skins.spark.mediaClasses.fullScreen.PlayPauseButtonSkin
 *  @see spark.skins.spark.mediaClasses.fullScreen.ScrubBarSkin
 *  @see spark.skins.spark.mediaClasses.fullScreen.ScrubBarThumbSkin
 *  @see spark.skins.spark.mediaClasses.fullScreen.ScrubBarTrackSkin
 *  @see spark.skins.spark.mediaClasses.fullScreen.VolumeBarSkin
 *  @see spark.skins.spark.mediaClasses.fullScreen.VolumeBarThumbSkin
 *  @see spark.skins.spark.mediaClasses.fullScreen.VolumeBarTrackSkin
 *  @see spark.skins.spark.mediaClasses.normal.FullScreenButtonSkin
 *  @see spark.skins.spark.mediaClasses.normal.MuteButtonSkin
 *  @see spark.skins.spark.mediaClasses.normal.PlayPauseButtonSkin
 *  @see spark.skins.spark.mediaClasses.normal.ScrubBarSkin
 *  @see spark.skins.spark.mediaClasses.normal.ScrubBarThumbSkin
 *  @see spark.skins.spark.mediaClasses.normal.ScrubBarTrackSkin
 *  @see spark.skins.spark.mediaClasses.normal.VolumeBarSkin
 *  @see spark.skins.spark.mediaClasses.normal.VolumeBarThumbSkin
 *  @see spark.skins.spark.mediaClasses.normal.VolumeBarTrackSkin
 *
 *  @mxml
 *
 *  <p>The <code>&lt;s:VideoPlayer&gt;</code> tag inherits all of the tag 
 *  attributes of its superclass and adds the following tag attributes:</p>
 *
 *  <pre>
 *  &lt;s:VideoPlayer
 
 *    <strong>Properties</strong>
 *    autoDisplayFirstFrame="true"
 *    autoPlay="true"
 *    autoRewind="true"
 *    loop="false"
 *    muted="false"
 *    pauseWhenHidden="true"
 *    scaleMode="letterbox"
 *    source=""
 *    volume="1"
 *  
 *    <strong>Events</strong>
 *    bytesLoadedChange="<i>No default</i>"
 *    complete="<i>No default</i>"
 *    currentTimeChange="<i>No default</i>"
 *    durationChange="<i>No default</i>"
 *    mediaPlayerStateChange="<i>No default</i>"
 *  
 * 
 *    <strong>Styles</strong>
 *    alignmentBaseline="baseline"
 *    baselineShift="0"
 *    cffHinting="0.0"
 *    color="0x000000"
 *    digitCase="default"
 *    digitWidth="default"
 *    direction="ltr"
 *    dominantBaseline="auto"
 *    dropShadowVisible="true"
 *    fontFamily="Arial"
 *    fontLookup="device"
 *    fontSize="12"
 *    fontStyle="normal"
 *    fontWeight="normal"
 *    justificationRule="auto"
 *    justificationStyle="auto"
 *    kerning="false"
 *    ligatureLevel="common"
 *    lineHeight="120%"
 *    lineThrough="false%"
 *    locale="en"
 *    renderingMode="cff"
 *    textAlign="start"
 *    textAlignLast="start"
 *    textAlpha="1"
 *    textDecoration="start"
 *    textJustify="interWord"
 *    trackingLeft="0"
 *    trackingRight="00"
 *    typographicCase="default"
 *  /&gt;
 *  </pre>
 *
 *  @includeExample examples/VideoPlayerExample.mxml
 *  
 *  @langversion 3.0
 *  @playerversion Flash 10
 *  @playerversion AIR 1.5
 *  @productversion Flex 4
 */
public class VideoPlayer extends SkinnableComponent
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
    private static const AUTO_DISPLAY_FIRST_FRAME_PROPERTY_FLAG:uint = 1 << 0;

    /**
     *  @private
     */
    private static const AUTO_PLAY_PROPERTY_FLAG:uint = 1 << 1;
    
    /**
     *  @private
     */
    private static const AUTO_REWIND_PROPERTY_FLAG:uint = 1 << 2;
    
    /**
     *  @private
     */
    private static const LOOP_PROPERTY_FLAG:uint = 1 << 3;
    
    /**
     *  @private
     */
    private static const SCALE_MODE_PROPERTY_FLAG:uint = 1 << 4;
    
    /**
     *  @private
     */
    private static const MUTED_PROPERTY_FLAG:uint = 1 << 5;
    
    /**
     *  @private
     */
    private static const SOURCE_PROPERTY_FLAG:uint = 1 << 6;
    
    /**
     *  @private
     */
    private static const VOLUME_PROPERTY_FLAG:uint = 1 << 7;
    
    /**
     *  @private
     */
    private static const PAUSE_WHEN_HIDDEN_PROPERTY_FLAG:uint = 1 << 8;
    
    /**
     *  @private
     */
    private static const THUMBNAIL_SOURCE_PROPERTY_FLAG:uint = 1 << 9;
    
    
    //--------------------------------------------------------------------------
    //
    //  Class properties
    //
    //--------------------------------------------------------------------------
    
    /**
     * @private
     */  
    private static var _screenClass:Class;
    
    /**
     * @private
     */
    private static var checkedForScreenClass:Boolean;
    
    /**
     *  @private
     */
    private static function get screenClass():Class
    {
        if (!checkedForScreenClass)
        {
            checkedForScreenClass = true;
            
            if (ApplicationDomain.currentDomain.
                hasDefinition("flash.display::Screen"))
            {
                _screenClass = Class(ApplicationDomain.currentDomain.
                    getDefinition("flash.display::Screen"));
            }
        }
        
        return _screenClass;
    }
    
    //--------------------------------------------------------------------------
    //
    //  Class mixins
    //
    //--------------------------------------------------------------------------

    /**
     *  @private
     *  Placeholder for mixin by VideoPlayerAccImpl.
     */
    mx_internal static var createAccessibilityImplementation:Function;

    //--------------------------------------------------------------------------
    //
    //  Constructor
    //
    //--------------------------------------------------------------------------
    
    /**
     *  Constructor.
     *   
     *  @langversion 3.0
     *  @playerversion Flash 10
     *  @playerversion AIR 1.5
     *  @productversion Flex 4
     */
    public function VideoPlayer()
    {
        super();
    }
    
    //--------------------------------------------------------------------------
    //
    //  Skin Parts
    //
    //--------------------------------------------------------------------------
    
    [SkinPart(required="true")]
    
    /**
     *  A required skin part that defines the VideoDisplay.
     *  
     *  @langversion 3.0
     *  @playerversion Flash 10
     *  @playerversion AIR 1.5
     *  @productversion Flex 4
     */
    public var videoDisplay:VideoDisplay;
    
    [SkinPart(required="false")]
    
    /**
     *  An optional skin part to display the current value of <code>codecurrentTime</code>.
     *  
     *  @langversion 3.0
     *  @playerversion Flash 10
     *  @playerversion AIR 1.5
     *  @productversion Flex 4
     */
    public var currentTimeDisplay:IDisplayText;
    
    [SkinPart(required="false")]
    
    /**
     *  An optional skin part for a button to toggle fullscreen mode.
     *  
     *  @langversion 3.0
     *  @playerversion Flash 10
     *  @playerversion AIR 1.5
     *  @productversion Flex 4
     */
    public var fullScreenButton:ButtonBase;
    
    [SkinPart(required="false")]
    
    /**
     *  An optional skin part for the mute button.  The mute 
     *  button has both a <code>muted</code> property and a 
     *  <code>volume</code> property.
     *  
     *  @langversion 3.0
     *  @playerversion Flash 10
     *  @playerversion AIR 1.5
     *  @productversion Flex 4
     */
    public var muteButton:MuteButton;
    
    [SkinPart(required="false")]
    
    /**
     *  An optional skin part for the pause button.
     *  
     *  @langversion 3.0
     *  @playerversion Flash 10
     *  @playerversion AIR 1.5
     *  @productversion Flex 4
     */
    public var pauseButton:ButtonBase;
    
    [SkinPart(required="false")]
    
    /**
     *  An optional skin part for the play button.
     *  
     *  @langversion 3.0
     *  @playerversion Flash 10
     *  @playerversion AIR 1.5
     *  @productversion Flex 4
     */
    public var playButton:ButtonBase;
    
    [SkinPart(required="false")]
    
    /**
     *  An optional skin part for all of the player controls.   
     *  This skin is used to determine what to hide when the player is in full screen 
     *  mode and there has been no user interaction.
     *  
     *  @langversion 3.0
     *  @playerversion Flash 10
     *  @playerversion AIR 1.5
     *  @productversion Flex 4
     */
    public var playerControls:DisplayObject;
    
    [SkinPart(required="false")]
    
    /**
     *  An optional skin part for a play/pause button.  When the 
     *  video is playing, the <code>selected</code> property is set to 
     *  <code>true</code>.  When the video is paused or stopped, 
     *  the <code>selected</code> property is set to <code>false</code>.
     *  
     *  @langversion 3.0
     *  @playerversion Flash 10
     *  @playerversion AIR 1.5
     *  @productversion Flex 4
     */
    public var playPauseButton:ToggleButtonBase;
    
    [SkinPart(required="false")]
    
    /**
     *  An optional skin part for the scrub bar (the 
     *  timeline).
     *  
     *  @langversion 3.0
     *  @playerversion Flash 10
     *  @playerversion AIR 1.5
     *  @productversion Flex 4
     */
    public var scrubBar:ScrubBar;
    
    [SkinPart(required="false")]
    
    /**
     *  An optional skin part for the stop button.
     *  
     *  @langversion 3.0
     *  @playerversion Flash 10
     *  @playerversion AIR 1.5
     *  @productversion Flex 4
     */
    public var stopButton:ButtonBase;
    
    [SkinPart(required="false")]
    
    /**
     *  An optional skin part to display the duration.
     *  
     *  @langversion 3.0
     *  @playerversion Flash 10
     *  @playerversion AIR 1.5
     *  @productversion Flex 4
     */
    public var durationDisplay:IDisplayText;
    
    [SkinPart(required="false")]
    
    /**
     *  An optional skin part for the volume control.
     *  
     *  @langversion 3.0
     *  @playerversion Flash 10
     *  @playerversion AIR 1.5
     *  @productversion Flex 4
     */
    public var volumeBar:VolumeBar;
    
    //--------------------------------------------------------------------------
    //
    //  Variables
    //
    //--------------------------------------------------------------------------
    
    /**
     *  @private
     *  Several properties are proxied to videoDisplay.  However, when videoDisplay
     *  is not around, we need to store values set on VideoPlayer.  This object 
     *  stores those values.  If videoDisplay is around, the values are stored 
     *  on the videoDisplay directly.  However, we need to know what values 
     *  have been set by the developer on the VideoPlayer (versus set on 
     *  the videoDisplay or defaults of the videoDisplay) as those are values 
     *  we want to carry around if the videoDisplay changes (via a new skin). 
     *  In order to store this info effeciently, videoDisplayProperties becomes 
     *  a uint to store a series of BitFlags.  These bits represent whether a 
     *  property has been explicitely set on this VideoPlayer.  When the 
     *  contentGroup is not around, videoDisplayProperties is a typeless 
     *  object to store these proxied properties.  When videoDisplay is around,
     *  videoDisplayProperties stores booleans as to whether these properties 
     *  have been explicitely set or not.
     */
    private var videoDisplayProperties:Object = {};
    
    /**
     *  @private
     *  The value of the pauseWhenHidden property before exiting 
     *  fullScreen.  We need to store it away here so we can 
     *  restore it at commitProperties() time because of an AIR 
     *  Mac bug.
     */
    private var exitingFullScreenPauseWhenHidden:Boolean;
    
    /**
     *  @private
     *  Whether the pauseWhenHidden property needs to be updated.
     */
    private var needsToUpdatePauseWhenHidden:Boolean = false;
    
    //--------------------------------------------------------------------------
    //
    //  Properties
    //
    //--------------------------------------------------------------------------
    
    //----------------------------------
    //  autoDisplayFirstFrame
    //----------------------------------
        
    [Inspectable(category="General", defaultValue="true")]
    
    /**
     *  @copy spark.components.VideoDisplay#autoDisplayFirstFrame
     * 
     *  @default true
     *  
     *  @langversion 3.0
     *  @playerversion Flash 10
     *  @playerversion AIR 1.5
     *  @productversion Flex 4
     */
    public function get autoDisplayFirstFrame():Boolean
    {
        if (videoDisplay)
        {
            return videoDisplay.autoDisplayFirstFrame;
        }
        else
        {
            var v:* = videoDisplayProperties.autoDisplayFirstFrame;
            return (v === undefined) ? true : v;
        }
    }
    
    /**
     * @private
     */
    public function set autoDisplayFirstFrame(value:Boolean):void
    {
        if (videoDisplay)
        {
            videoDisplay.autoDisplayFirstFrame = value;
            videoDisplayProperties = BitFlagUtil.update(videoDisplayProperties as uint, 
                AUTO_DISPLAY_FIRST_FRAME_PROPERTY_FLAG, true);
        }
        else
        {
            videoDisplayProperties.autoDisplayFirstFrame = value;
        }
    }
    
    //----------------------------------
    //  autoPlay
    //----------------------------------
    
    [Inspectable(category="General", defaultValue="true")]
    
    /**
     *  @copy spark.components.VideoDisplay#autoPlay
     * 
     *  @default true
     * 
     *  @langversion 3.0
     *  @playerversion Flash 10
     *  @playerversion AIR 1.5
     *  @productversion Flex 4
     */
    public function get autoPlay():Boolean
    {
        if (videoDisplay)
        {
            return videoDisplay.autoPlay;
        }
        else
        {
            var v:* = videoDisplayProperties.autoPlay;
            return (v === undefined) ? true : v;
        }
    }
    
    /**
     * @private
     */
    public function set autoPlay(value:Boolean):void
    {
        if (videoDisplay)
        {
            videoDisplay.autoPlay = value;
            videoDisplayProperties = BitFlagUtil.update(videoDisplayProperties as uint, 
                AUTO_PLAY_PROPERTY_FLAG, true);
        }
        else
        {
            videoDisplayProperties.autoPlay = value;
        }
    }
    
    //----------------------------------
    //  autoRewind
    //----------------------------------
    
    [Inspectable(category="General", defaultValue="true")]
    
    /**
     *  @copy spark.components.VideoDisplay#autoRewind
     * 
     *  @default true
     * 
     *  @langversion 3.0
     *  @playerversion Flash 10
     *  @playerversion AIR 1.5
     *  @productversion Flex 4
     */
    public function get autoRewind():Boolean
    {
        if (videoDisplay)
        {
            return videoDisplay.autoRewind;
        }
        else
        {
            var v:* = videoDisplayProperties.autoRewind;
            return (v === undefined) ? true : v;
        }
    }
    
    /**
     * @private
     */
    public function set autoRewind(value:Boolean):void
    {
        if (videoDisplay)
        {
            videoDisplay.autoRewind = value;
            videoDisplayProperties = BitFlagUtil.update(videoDisplayProperties as uint, 
                AUTO_REWIND_PROPERTY_FLAG, true);
        }
        else
        {
            videoDisplayProperties.autoRewind = value;
        }
    }
    
    //----------------------------------
    //  bytesLoaded
    //----------------------------------
    
    [Inspectable(Category="General", defaultValue="0")]
    [Bindable("bytesLoadedChange")]
    [Bindable("mediaPlayerStateChange")]
    
    /**
     *  @copy spark.components.VideoDisplay#bytesLoaded
     * 
     *  @default 0
     * 
     *  @langversion 3.0
     *  @playerversion Flash 10
     *  @playerversion AIR 1.5
     *  @productversion Flex 4
     */
    public function get bytesLoaded():Number
    {
        if (videoDisplay)
            return videoDisplay.bytesLoaded;
        else
            return 0;
    }
    
    //----------------------------------
    //  bytesTotal
    //----------------------------------
    
    [Inspectable(Category="General", defaultValue="0")]
    [Bindable("mediaPlayerStateChange")]
    
    /**
     *  @copy spark.components.VideoDisplay#bytesTotal
     * 
     *  @default 0
     * 
     *  @langversion 3.0
     *  @playerversion Flash 10
     *  @playerversion AIR 1.5
     *  @productversion Flex 4
     */
    public function get bytesTotal():Number
    {
        if (videoDisplay)
            return videoDisplay.bytesTotal;
        else
            return 0;
    }
    
    //----------------------------------
    //  currentTime
    //----------------------------------
    
    [Inspectable(Category="General", defaultValue="0")]
    [Bindable("currentTimeChange")]
    [Bindable("mediaPlayerStateChange")]
    
    /**
     *  @copy spark.components.VideoDisplay#currentTime
     * 
     *  @default 0
     * 
     *  @langversion 3.0
     *  @playerversion Flash 10
     *  @playerversion AIR 1.5
     *  @productversion Flex 4
     */
    public function get currentTime():Number
    {
        if (videoDisplay)
            return videoDisplay.currentTime;
        else
            return 0;
    }
    
    //----------------------------------
    //  duration
    //----------------------------------
    
    [Inspectable(Category="General", defaultValue="0")]
    [Bindable("durationChange")]
    [Bindable("mediaPlayerStateChange")]
    
    /**
     *  @copy spark.components.VideoDisplay#duration
     * 
     *  @default 0
     * 
     *  @langversion 3.0
     *  @playerversion Flash 10
     *  @playerversion AIR 1.5
     *  @productversion Flex 4
     */
    public function get duration():Number
    {
        if (videoDisplay)
            return videoDisplay.duration;
        else
            return 0;
    }
    
    //----------------------------------
    //  loop
    //----------------------------------
    
    [Inspectable(Category="General", defaultValue="false")]
    
    /**
     *  @copy spark.components.VideoDisplay#loop
     * 
     *  @default false
     * 
     *  @langversion 3.0
     *  @playerversion Flash 10
     *  @playerversion AIR 1.5
     *  @productversion Flex 4
     */
    public function get loop():Boolean
    {
        if (videoDisplay)
        {
            return videoDisplay.loop;
        }
        else
        {
            var v:* = videoDisplayProperties.loop;
            return (v === undefined) ? false : v;
        }
    }
    
    /**
     *  @private
     */
    public function set loop(value:Boolean):void
    {
        if (videoDisplay)
        {
            videoDisplay.loop = value;
            videoDisplayProperties = BitFlagUtil.update(videoDisplayProperties as uint, 
                LOOP_PROPERTY_FLAG, true);
        }
        else
        {
            videoDisplayProperties.loop = value;
        }
    }
    
    //----------------------------------
    //  mediaPlayerState
    //----------------------------------
    
    [Inspectable(category="General", defaultValue="uninitialized")]
    [Bindable("mediaPlayerStateChange")]
    
    /**
     *  @copy spark.components.VideoDisplay#mediaPlayerState
     *  
     *  @default uninitialized
     * 
     *  @see org.osmf.media.MediaPlayerState
     * 
     *  @langversion 3.0
     *  @playerversion Flash 10
     *  @playerversion AIR 1.5
     *  @productversion Flex 4
     */
    public function get mediaPlayerState():String
    {
        if (videoDisplay)
            return videoDisplay.mediaPlayerState;
        else
            return MediaPlayerState.UNINITIALIZED;
    }
    
    //----------------------------------
    //  muted
    //----------------------------------
    
    [Inspectable(category="General", defaultValue="false")]
    [Bindable("volumeChanged")]
    
    /**
     *  @copy spark.components.VideoDisplay#muted
     * 
     *  @default false
     * 
     *  @langversion 3.0
     *  @playerversion Flash 10
     *  @playerversion AIR 1.5
     *  @productversion Flex 4
     */
    public function get muted():Boolean
    {
        if (videoDisplay)
        {
            return videoDisplay.muted;
        }
        else
        {
            var v:* = videoDisplayProperties.muted;
            return (v === undefined) ? false : v;
        }
    }
    
    /**
     *  @private
     */
    public function set muted(value:Boolean):void
    {
        if (videoDisplay)
        {
            videoDisplay.muted = value;
            videoDisplayProperties = BitFlagUtil.update(videoDisplayProperties as uint, 
                MUTED_PROPERTY_FLAG, true);
        }
        else
        {
            videoDisplayProperties.muted = value;
        }
        
        if (volumeBar)
            volumeBar.muted = value;
        if (muteButton)
            muteButton.muted = value;
    }
    
    //----------------------------------
    //  pauseWhenHidden
    //----------------------------------
    
    [Inspectable(category="General", defaultValue="true")]
    
    /**
     *  @copy spark.components.VideoDisplay#pauseWhenHidden
     * 
     *  @default true
     *  
     *  @langversion 3.0
     *  @playerversion Flash 10
     *  @playerversion AIR 1.5
     *  @productversion Flex 4
     */
    public function get pauseWhenHidden():Boolean
    {
        if (needsToUpdatePauseWhenHidden)
        {
            return exitingFullScreenPauseWhenHidden;
        }
        else if (videoDisplay)
        {
            return videoDisplay.pauseWhenHidden;
        }
        else
        {
            var v:* = videoDisplayProperties.pauseWhenHidden;
            return (v === undefined) ? false : v;
        }
    }
    
    /**
     *  @private
     */
    public function set pauseWhenHidden(value:Boolean):void
    {
        if (needsToUpdatePauseWhenHidden)
        {
            exitingFullScreenPauseWhenHidden = value;
        }
        else if (videoDisplay)
        {
            videoDisplay.pauseWhenHidden = value;
            videoDisplayProperties = BitFlagUtil.update(videoDisplayProperties as uint, 
                PAUSE_WHEN_HIDDEN_PROPERTY_FLAG, true);
        }
        else
        {
            videoDisplayProperties.pauseWhenHidden = value;
        }
    }
    
    //----------------------------------
    //  playing
    //----------------------------------
    
    [Inspectable(category="General")]
    [Bindable("mediaPlayerStateChange")]
    
    /**
     *  @copy spark.components.VideoDisplay#playing
     * 
     *  @langversion 3.0
     *  @playerversion Flash 10
     *  @playerversion AIR 1.5
     *  @productversion Flex 4
     */
    public function get playing():Boolean
    {
        if (videoDisplay)
            return videoDisplay.playing;
        else
            return false;
    }
    
    //----------------------------------
    //  scaleMode
    //----------------------------------
    
    [Inspectable(Category="General", enumeration="none,stretch,letterbox,zoom", defaultValue="letterbox")]
    
    /**
     *  @copy spark.components.VideoDisplay#scaleMode
     * 
     *  @default "letterbox"
     *
     *  @see org.osmf.display.ScaleMode
     * 
     *  @langversion 3.0
     *  @playerversion Flash 10
     *  @playerversion AIR 1.5
     *  @productversion Flex 4
     */
    public function get scaleMode():String
    {
        if (videoDisplay)
        {
            return videoDisplay.scaleMode;
        }
        else
        {
            var v:* = videoDisplayProperties.scaleMode;
            return (v === undefined) ? "letterbox" : v;
        }
    }
    
    /**
     *  @private
     */
    public function set scaleMode(value:String):void
    {
        if (videoDisplay)
        {
            videoDisplay.scaleMode = value;
            videoDisplayProperties = BitFlagUtil.update(videoDisplayProperties as uint, 
                SCALE_MODE_PROPERTY_FLAG, true);
        }
        else
        {
            videoDisplayProperties.scaleMode = value;
        }
    }
    
    //----------------------------------
    //  source
    //----------------------------------
    
    [Inspectable(category="General", defaultValue="null")]
    [Bindable("sourceChanged")]
    
    /**
     *  @copy spark.components.VideoDisplay#source
     * 
     *  @default null
     * 
     *  @langversion 3.0
     *  @playerversion Flash 10
     *  @playerversion AIR 1.5
     *  @productversion Flex 4
     */
    public function get source():Object
    {
        if (videoDisplay)
        {
            return videoDisplay.source;
        }
        else
        {
            var v:* = videoDisplayProperties.source;
            return (v === undefined) ? null : v;
        }
    }
    
    /**
     * @private
     */
    public function set source(value:Object):void
    {
        if (videoDisplay)
        {
            videoDisplay.source = value;
            videoDisplayProperties = BitFlagUtil.update(videoDisplayProperties as uint, 
                SOURCE_PROPERTY_FLAG, true);
        }
        else
        {
            videoDisplayProperties.source = value;
        }
    }
    
    //----------------------------------
    //  thumbnailSource
    //----------------------------------
    
    [Inspectable(category="General")]
    
    /**
     *  @private
     *  @copy spark.components.VideoDisplay#thumbnailSource
     * 
     *  @langversion 3.0
     *  @playerversion Flash 10
     *  @playerversion AIR 1.5
     *  @productversion Flex 4
     */
    mx_internal function get thumbnailSource():Object
    {
        if (videoDisplay)
        {
            return videoDisplay.thumbnailSource;
        }
        else
        {
            var v:* = videoDisplayProperties.thumbnailSource;
            return (v === undefined) ? null : v;
        }
    }
    
    /**
     * @private
     */
    mx_internal function set thumbnailSource(value:Object):void
    {
        if (videoDisplay)
        {
            videoDisplay.thumbnailSource = value;
            videoDisplayProperties = BitFlagUtil.update(videoDisplayProperties as uint, 
                THUMBNAIL_SOURCE_PROPERTY_FLAG, true);
        }
        else
        {
            videoDisplayProperties.thumbnailSource = value;
        }
    }
    
    //----------------------------------
    //  videoObject
    //----------------------------------
    
    [Inspectable(category="General", defaultValue="null")]
    
    /**
     *  @copy spark.components.VideoDisplay#videoObject
     * 
     *  @default null
     *  
     *  @langversion 3.0
     *  @playerversion Flash 10
     *  @playerversion AIR 1.5
     *  @productversion Flex 4
     */
    public function get videoObject():Video
    {
        if (videoDisplay)
            return videoDisplay.videoObject;
        else
            return null;
    }
    
    //----------------------------------
    //  volume
    //----------------------------------
    
    [Inspectable(category="General", defaultValue="1.0", minValue="0.0", maxValue="1.0")]
    [Bindable("volumeChanged")]
    
    /**
     *  @copy spark.components.VideoDisplay#volume
     * 
     *  @default 1
     * 
     *  @langversion 3.0
     *  @playerversion Flash 10
     *  @playerversion AIR 1.5
     *  @productversion Flex 4
     */
    public function get volume():Number
    {
        if (videoDisplay)
        {
            return videoDisplay.volume;
        }
        else
        {
            var v:* = videoDisplayProperties.volume;
            return (v === undefined) ? 1 : v;
        }
    }
    
    /**
     * @private
     */
    public function set volume(value:Number):void
    {
        if (videoDisplay)
        {
            videoDisplay.volume = value;
            videoDisplayProperties = BitFlagUtil.update(videoDisplayProperties as uint, 
                VOLUME_PROPERTY_FLAG, true);
        }
        else
        {
            videoDisplayProperties.volume = value;
        }
        
        if (volumeBar)
            volumeBar.value = value;
    }
    
    //--------------------------------------------------------------------------
    //
    //  Overridden methods
    //
    //--------------------------------------------------------------------------
  
    /**
     *  @private
     */
    override protected function initializeAccessibility():void
    {
        if (VideoPlayer.createAccessibilityImplementation != null)
            VideoPlayer.createAccessibilityImplementation(this);
    }

    /**
     *  @private
     */
    override protected function getCurrentSkinState():String
    {   
        if (!videoDisplay || !videoDisplay.videoPlayer)
            return null;
        
        var state:String = videoDisplay.videoPlayer.state;
        
        // now that we have our video player's current state (atleast the one we care about)
        // and that we've set the previous state to something we care about, let's figure 
        // out our skin's state
        
        if (!enabled)
            state="disabled"
        
        if (fullScreen)
            return state + "AndFullScreen";
        
        return state;
    }
    
    /**
     *  @private
     */
    override protected function partAdded(partName:String, instance:Object):void
    {
        super.partAdded(partName, instance);

        if (instance == videoDisplay)
        {
            videoDisplay.addEventListener(TimeEvent.CURRENT_TIME_CHANGE, videoDisplay_currentTimeChangeHandler);
            videoDisplay.addEventListener(LoadEvent.BYTES_LOADED_CHANGE, videoDisplay_bytesLoadedChangeHandler);
            videoDisplay.addEventListener(MediaPlayerStateChangeEvent.MEDIA_PLAYER_STATE_CHANGE, videoDisplay_mediaPlayerStateChangeHandler);
            videoDisplay.addEventListener(TimeEvent.DURATION_CHANGE, videoDisplay_durationChangeHandler);
            videoDisplay.addEventListener(TimeEvent.COMPLETE, dispatchEvent);
            
            // just strictly for binding purposes
            videoDisplay.addEventListener("sourceChanged", dispatchEvent);
            videoDisplay.addEventListener("volumeChanged", videoDisplay_volumeChangedHandler);
            
            // copy proxied values from videoProperties (if set) to video
            
            var newVideoProperties:uint = 0;
            
            if (videoDisplayProperties.source !== undefined)
            {
                videoDisplay.source = videoDisplayProperties.source;
                newVideoProperties = BitFlagUtil.update(newVideoProperties as uint, 
                    SOURCE_PROPERTY_FLAG, true);
            }
            
            if (videoDisplayProperties.autoPlay !== undefined)
            {
                videoDisplay.autoPlay = videoDisplayProperties.autoPlay;
                newVideoProperties = BitFlagUtil.update(newVideoProperties as uint, 
                    AUTO_PLAY_PROPERTY_FLAG, true);
            }
            
            if (videoDisplayProperties.volume !== undefined)
            {
                videoDisplay.volume = videoDisplayProperties.volume;
                newVideoProperties = BitFlagUtil.update(newVideoProperties as uint, 
                    VOLUME_PROPERTY_FLAG, true);
            }
            
            if (videoDisplayProperties.autoRewind !== undefined)
            {
                videoDisplay.autoRewind = videoDisplayProperties.autoRewind;
                newVideoProperties = BitFlagUtil.update(newVideoProperties as uint, 
                    AUTO_REWIND_PROPERTY_FLAG, true);
            }
            
            if (videoDisplayProperties.loop !== undefined)
            {
                videoDisplay.loop = videoDisplayProperties.loop;
                newVideoProperties = BitFlagUtil.update(newVideoProperties as uint, 
                    LOOP_PROPERTY_FLAG, true);
            }
            
            if (videoDisplayProperties.scaleMode !== undefined)
            {
                videoDisplay.scaleMode = videoDisplayProperties.scaleMode;
                newVideoProperties = BitFlagUtil.update(newVideoProperties as uint, 
                    SCALE_MODE_PROPERTY_FLAG, true);
            }
            
            if (videoDisplayProperties.muted !== undefined)
            {
                videoDisplay.muted = videoDisplayProperties.muted;
                newVideoProperties = BitFlagUtil.update(newVideoProperties as uint, 
                    MUTED_PROPERTY_FLAG, true);
            }
            
            if (videoDisplayProperties.pauseWhenHidden !== undefined)
            {
                videoDisplay.pauseWhenHidden = videoDisplayProperties.pauseWhenHidden;
                newVideoProperties = BitFlagUtil.update(newVideoProperties as uint, 
                    PAUSE_WHEN_HIDDEN_PROPERTY_FLAG, true);
            }
            
            if (videoDisplayProperties.autoDisplayFirstFrame !== undefined)
            {
                videoDisplay.autoDisplayFirstFrame = videoDisplayProperties.autoDisplayFirstFrame;
                newVideoProperties = BitFlagUtil.update(newVideoProperties as uint, 
                    AUTO_DISPLAY_FIRST_FRAME_PROPERTY_FLAG, true);
            }
            
            if (videoDisplayProperties.thumbnailSource !== undefined)
            {
                videoDisplay.thumbnailSource = videoDisplayProperties.thumbnailSource;
                newVideoProperties = BitFlagUtil.update(newVideoProperties as uint, 
                    THUMBNAIL_SOURCE_PROPERTY_FLAG, true);
            }
            
            // these are state properties just carried over from an old video element
            if (videoDisplayProperties.currentTime !== undefined || 
                videoDisplayProperties.playing !== undefined)
            {
                videoDisplay_updateCompleteHandlerProperties = {
                    autoPlay: videoDisplay.autoPlay,
                        playing: videoDisplayProperties.playing,
                        currentTime: videoDisplayProperties.currentTime};
                
                // so the videoDisplay doesn't start playing...we'll handle it instead in 
                // videoDisplay_updateCompleteHandler
                videoDisplay.autoPlay = false;
                
                videoDisplay.addEventListener(FlexEvent.UPDATE_COMPLETE, videoDisplay_updateCompleteHandler);
            }
            
            videoDisplayProperties = newVideoProperties;
            
            if (volumeBar)
            {
                volumeBar.value = videoDisplay.volume;
                volumeBar.muted = videoDisplay.muted;
            }
            
            if (muteButton)
            {
                muteButton.volume = videoDisplay.volume;
                muteButton.muted = videoDisplay.muted;
            }
            
            if (scrubBar)
                updateScrubBar();
            
            if (currentTimeDisplay)
                updateCurrentTime();
            
            if (durationDisplay)
                updateDuration();
        }
        else if (instance == playButton)
        {
            playButton.addEventListener(MouseEvent.CLICK, playButton_clickHandler);
        }
        else if (instance == pauseButton)
        {
            pauseButton.addEventListener(MouseEvent.CLICK, pauseButton_clickHandler);
        }
        else if (instance == playPauseButton)
        {
            playPauseButton.addEventListener(MouseEvent.CLICK, playPauseButton_clickHandler);
        }
        else if (instance == stopButton)
        {
            stopButton.addEventListener(MouseEvent.CLICK, stopButton_clickHandler);
        }
        else if (instance == muteButton)
        {
            if (videoDisplay)
            {
                muteButton.muted = muted;
                muteButton.volume = volume;
            }
            
            muteButton.addEventListener(FlexEvent.MUTED_CHANGE, muteButton_mutedChangeHandler);
        }
        else if (instance == volumeBar)
        {
            volumeBar.minimum = 0;
            volumeBar.maximum = 1;
            if (videoDisplay)
            {
                volumeBar.value = volume;
                volumeBar.muted = muted;
            }
            
            volumeBar.addEventListener(Event.CHANGE, volumeBar_changeHandler);
            volumeBar.addEventListener(FlexEvent.MUTED_CHANGE, volumeBar_mutedChangeHandler);
        }
        else if (instance == scrubBar)
        {
            if (videoDisplay)
                updateScrubBar();
            
            // add thumbPress and thumbRelease so we pause the video while dragging
            scrubBar.addEventListener(TrackBaseEvent.THUMB_PRESS, scrubBar_thumbPressHandler);
            scrubBar.addEventListener(TrackBaseEvent.THUMB_RELEASE, scrubBar_thumbReleaseHandler);
            
            // add change to actually seek() when the change is complete
            scrubBar.addEventListener(Event.CHANGE, scrubBar_changeHandler);
            
            // add changeEnd and changeStart so we don't update the scrubbar's value 
            // while the scrubbar is moving around due to an animation
            scrubBar.addEventListener(FlexEvent.CHANGE_END, scrubBar_changeEndHandler);
            scrubBar.addEventListener(FlexEvent.CHANGE_START, scrubBar_changeStartHandler);
        }
        else if (instance == fullScreenButton)
        {
            fullScreenButton.addEventListener(MouseEvent.CLICK, fullScreenButton_clickHandler);
        }
        else if (instance == currentTimeDisplay)
        {
            if (videoDisplay)
                updateCurrentTime();
        }
        else if (instance == durationDisplay)
        {
            if (videoDisplay)
                updateDuration();
        }
    }
    
    /**
     *  @private
     *  Holds the state of the video element when the skin is being swapped out.
     *  This is so the new videoDisplay can load up and start playing 
     *  where it left off.
     */
    private var videoDisplay_updateCompleteHandlerProperties:Object;
    
    /**
     *  @private
     *  We only listen for the updateComplete event on the videoDisplay when 
     *  a skin has been swapped.  This is so we can push the old videoDisplay's 
     *  state in to the new object, and we can start playing the video 
     *  where it left off.
     */
    private function videoDisplay_updateCompleteHandler(event:FlexEvent):void
    {
        if (videoDisplay_updateCompleteHandlerProperties.autoPlay)
            videoDisplay.autoPlay = true;
        
        if (videoDisplay_updateCompleteHandlerProperties.currentTime !== undefined)
            videoDisplay.seek(videoDisplay_updateCompleteHandlerProperties.currentTime);
        
        if (videoDisplay_updateCompleteHandlerProperties.playing)
            videoDisplay.play();
        
        videoDisplay_updateCompleteHandlerProperties = null;
        videoDisplay.removeEventListener(FlexEvent.UPDATE_COMPLETE, videoDisplay_updateCompleteHandler);
    }
    
    /**
     *  @private
     */
    override protected function partRemoved(partName:String, instance:Object):void
    {
        super.partRemoved(partName, instance);

        if (instance == videoDisplay)
        {
            // validate before doing anything with the videoDisplay.
            // This is so if the video element hasn't been validated, it won't start playing.
            // plus this way we'll get a valid currentTime and all those other properties 
            // we are interested in.
            videoDisplay.validateNow();
            
            // copy proxied values from video (if explicitely set) to videoProperties
            
            var newVideoProperties:Object = {};
            
            if (BitFlagUtil.isSet(videoDisplayProperties as uint, SOURCE_PROPERTY_FLAG))
                newVideoProperties.source = videoDisplay.source;
            
            if (BitFlagUtil.isSet(videoDisplayProperties as uint, AUTO_PLAY_PROPERTY_FLAG))
                newVideoProperties.autoPlay = videoDisplay.autoPlay;
            
            if (BitFlagUtil.isSet(videoDisplayProperties as uint, VOLUME_PROPERTY_FLAG))
                newVideoProperties.volume = videoDisplay.volume;
            
            if (BitFlagUtil.isSet(videoDisplayProperties as uint, AUTO_REWIND_PROPERTY_FLAG))
                newVideoProperties.autoRewind = videoDisplay.autoRewind;
            
            if (BitFlagUtil.isSet(videoDisplayProperties as uint, LOOP_PROPERTY_FLAG))
                newVideoProperties.loop = videoDisplay.loop;
            
            if (BitFlagUtil.isSet(videoDisplayProperties as uint, SCALE_MODE_PROPERTY_FLAG))
                newVideoProperties.scaleMode = videoDisplay.scaleMode;
            
            if (BitFlagUtil.isSet(videoDisplayProperties as uint, MUTED_PROPERTY_FLAG))
                newVideoProperties.muted = videoDisplay.muted;
            
            if (BitFlagUtil.isSet(videoDisplayProperties as uint, PAUSE_WHEN_HIDDEN_PROPERTY_FLAG))
                newVideoProperties.pauseWhenHidden = videoDisplay.pauseWhenHidden;
            
            if (BitFlagUtil.isSet(videoDisplayProperties as uint, AUTO_DISPLAY_FIRST_FRAME_PROPERTY_FLAG))
                newVideoProperties.autoDisplayFirstFrame = videoDisplay.autoDisplayFirstFrame;
            
            if (BitFlagUtil.isSet(videoDisplayProperties as uint, THUMBNAIL_SOURCE_PROPERTY_FLAG))
                newVideoProperties.thumbnailSource = videoDisplay.thumbnailSource;
            
            // push our current state (where we were in the video and whether we were playing)
            // so that the new skin gets these as well
            newVideoProperties.currentTime = videoDisplay.currentTime;
            newVideoProperties.playing = videoDisplay.playing;
            
            videoDisplay.stop();
            
            videoDisplayProperties = newVideoProperties;
            
            videoDisplay.removeEventListener(TimeEvent.CURRENT_TIME_CHANGE, videoDisplay_currentTimeChangeHandler);
            videoDisplay.removeEventListener(LoadEvent.BYTES_LOADED_CHANGE, videoDisplay_bytesLoadedChangeHandler);
            videoDisplay.removeEventListener(MediaPlayerStateChangeEvent.MEDIA_PLAYER_STATE_CHANGE, videoDisplay_mediaPlayerStateChangeHandler);
            videoDisplay.removeEventListener(TimeEvent.DURATION_CHANGE, videoDisplay_durationChangeHandler);
            videoDisplay.removeEventListener(TimeEvent.COMPLETE, dispatchEvent);
            
            // just strictly for binding purposes
            videoDisplay.removeEventListener("sourceChanged", dispatchEvent);
            videoDisplay.removeEventListener("volumeChanged", videoDisplay_volumeChangedHandler);
        }
        else if (instance == playButton)
        {
            playButton.removeEventListener(MouseEvent.CLICK, playButton_clickHandler);
        }
        else if (instance == pauseButton)
        {
            pauseButton.removeEventListener(MouseEvent.CLICK, pauseButton_clickHandler);
        }
        else if (instance == playPauseButton)
        {
            playPauseButton.removeEventListener(MouseEvent.CLICK, playPauseButton_clickHandler);
        }
        else if (instance == stopButton)
        {
            stopButton.removeEventListener(MouseEvent.CLICK, stopButton_clickHandler);
        }
        else if (instance == muteButton)
        {
            playButton.removeEventListener(FlexEvent.MUTED_CHANGE, muteButton_mutedChangeHandler);
        }
        else if (instance == volumeBar)
        {
            volumeBar.removeEventListener(Event.CHANGE, volumeBar_changeHandler);
            volumeBar.removeEventListener(FlexEvent.MUTED_CHANGE, volumeBar_mutedChangeHandler);
        }
        else if (instance == scrubBar)
        {
            scrubBar.removeEventListener(TrackBaseEvent.THUMB_PRESS, scrubBar_thumbPressHandler);
            scrubBar.removeEventListener(TrackBaseEvent.THUMB_RELEASE, scrubBar_thumbReleaseHandler);
            scrubBar.removeEventListener(Event.CHANGE, scrubBar_changeHandler);
            scrubBar.removeEventListener(FlexEvent.CHANGE_END, scrubBar_changeEndHandler);
            scrubBar.removeEventListener(FlexEvent.CHANGE_START, scrubBar_changeStartHandler);
        }
        else if (instance == fullScreenButton)
        {
            fullScreenButton.removeEventListener(MouseEvent.CLICK, fullScreenButton_clickHandler);
        }
    }
    
    /**
     *  @private
     */
    override protected function commitProperties():void
    {
        super.commitProperties();
        
        // if coming from full screen mode, we reset the 
        // pauseWhenHidden property here because of an AIR bug
        // that requires us to defer it.
        if (needsToUpdatePauseWhenHidden)
        {
            needsToUpdatePauseWhenHidden = false;
            pauseWhenHidden = exitingFullScreenPauseWhenHidden;
        }
    }
    
    //--------------------------------------------------------------------------
    //
    //  Methods
    //
    //--------------------------------------------------------------------------
    
    /**
     *  @throws TypeError If the skin hasn't been loaded and there is no videoDisplay.    
     *
     *  @copy spark.components.VideoDisplay#pause()
     * 
     * 
     *  @langversion 3.0
     *  @playerversion Flash 10
     *  @playerversion AIR 1.5
     *  @productversion Flex 4
     */
    public function pause():void
    {
        videoDisplay.pause();
    }
    
    /**
     *  @copy spark.components.VideoDisplay#play()
     * 
     *  @throws TypeError if the skin hasn't been loaded up yet
     *                    and there's no videoDisplay.
     * 
     *  @langversion 3.0
     *  @playerversion Flash 10
     *  @playerversion AIR 1.5
     *  @productversion Flex 4
     */
    public function play():void
    {
        //trace("play");
        videoDisplay.play();
    }
    
    /**
     *  @copy spark.components.VideoDisplay#seek()
     * 
     *  @throws TypeError if the skin hasn't been loaded up yet
     *                    and there's no videoDisplay.
     * 
     *  @langversion 3.0
     *  @playerversion Flash 10
     *  @playerversion AIR 1.5
     *  @productversion Flex 4
     */
    public function seek(time:Number):void
    {
        videoDisplay.seek(time);
    }
    
    /**
     *  @copy spark.components.VideoDisplay#stop()
     * 
     *  @throws TypeError if the skin hasn't been loaded up yet
     *                    and there's no videoDisplay.
     * 
     *  @langversion 3.0
     *  @playerversion Flash 10
     *  @playerversion AIR 1.5
     *  @productversion Flex 4
     */
    public function stop():void
    {
        videoDisplay.stop();
    }
    
    /**
     *  @private
     */
    private function updateScrubBar():void
    {
        if (!videoDisplay)
            return;
        
        if (!scrubBarMouseCaptured && !scrubBarChanging)
        {
            scrubBar.minimum = 0;
            scrubBar.maximum = videoDisplay.duration;
            scrubBar.value = videoDisplay.currentTime;
        }
        
        // if streaming, then we pretend to have everything in view
        // if progressive, then look at the bytesLoaded and bytesTotal
        if (!videoDisplay.videoPlayer.canLoad)
            scrubBar.loadedRangeEnd = videoDisplay.duration;
        else if (videoDisplay.bytesTotal == 0)
            scrubBar.loadedRangeEnd = 0;
        else
            scrubBar.loadedRangeEnd = (videoDisplay.bytesLoaded/videoDisplay.bytesTotal)*videoDisplay.duration;
    }
    
    /**
     *  @private
     */
    private function updateDuration():void
    {
        durationDisplay.text = formatTimeValue(duration);
    }
    
    /**
     *  Formats a time value, specified in seconds, into a String that 
     *  gets used for <code>currentTime</code> and the <code>duration</code>.
     * 
     *  @param value Value in seconds of the time to format.
     * 
     *  @return Formatted time value.
     *  
     *  @langversion 3.0
     *  @playerversion Flash 10
     *  @playerversion AIR 2.5
     *  @productversion Flex 4.5
     */
    protected function formatTimeValue(value:Number):String
    {
        // default format: hours:minutes:seconds
        value = Math.round(value);
        
        var hours:uint = Math.floor(value/3600) % 24;
        var minutes:uint = Math.floor(value/60) % 60;
        var seconds:uint = value % 60;
        
        var result:String = "";
        if (hours != 0)
            result = hours + ":";
        
        if (result && minutes < 10)
            result += "0" + minutes + ":";
        else
            result += minutes + ":";
        
        if (seconds < 10)
            result += "0" + seconds;
        else
            result += seconds;
        
        return result;
    }
    
    /**
     *  @private
     */
    private function updateCurrentTime():void
    {
        currentTimeDisplay.text = formatTimeValue(currentTime);
    } 
        
    /**
     *  @private
     *  Returns the screen bounds. 
     *  If we are on the AIR Player, we need to work around AIR Player bug #2503351 
     *  We check if the flash.display.Screen class is defined. If so, then 
     *  we are running on the AIR Player and can access this API. 
     */
    mx_internal function getScreenBounds():Rectangle
    {       
        var resultRect:Rectangle = new Rectangle(0, 0, stage.fullScreenWidth, stage.fullScreenHeight);
        
        if (screenClass)
        {
            // Get the screen where the application resides
            try 
            {
                var nativeWindowBounds:Rectangle = stage["nativeWindow"]["bounds"];             
                var currentScreen:Object = screenClass["getScreensForRectangle"](nativeWindowBounds)[0];
          
                // Return the bounds of that screen
                resultRect = currentScreen["bounds"];
            }
            catch (e:Error)
            {
            }
        }
        
        return resultRect;
    }
    
    //--------------------------------------------------------------------------
    //
    //  Event handlers
    //
    //--------------------------------------------------------------------------

    /**
     *  @private
     */
    private function videoDisplay_currentTimeChangeHandler(event:TimeEvent):void
    {
        if (scrubBar)
            updateScrubBar();
        
        if (currentTimeDisplay)
            updateCurrentTime();
        
        dispatchEvent(event);
    }
    
    /**
     *  @private
     */
    private function videoDisplay_bytesLoadedChangeHandler(event:LoadEvent):void
    {
        if (scrubBar)
            updateScrubBar();
        
        dispatchEvent(event);
    }
    
    /**
     *  @private
     */
    private function videoDisplay_mediaPlayerStateChangeHandler(event:MediaPlayerStateChangeEvent):void
    {
        invalidateSkinState();
        
        if (scrubBar)
            updateScrubBar();
        
        if (durationDisplay)
            updateDuration();
        
        if (currentTimeDisplay)
            updateCurrentTime();
        
        if (playPauseButton)
            playPauseButton.selected = playing;
        
        //trace("mediaPlayerStateChangeHandler " + event + " state = " + event.state + " playing = " + playing);
        
        dispatchEvent(event);
    }
    
    /**
     *  @private
     */
    private function videoDisplay_durationChangeHandler(event:TimeEvent):void
    {
        if (scrubBar)
            updateScrubBar();
        
        if (durationDisplay)
            updateDuration();
        
        dispatchEvent(event);
    }
    
    /**
     *  @private
     */
    private function videoDisplay_volumeChangedHandler(event:Event):void
    {
        if (volumeBar)
        {
            volumeBar.value = volume;
            volumeBar.muted = muted;
        }
        
        if (muteButton)
        {
            muteButton.muted = muted;
            muteButton.volume = volume;
        }
        
        dispatchEvent(event);
    }
    
    /**
     *  @private
     *  Indicates whether we are in the full screen state or not.
     *  We use this when determining our current skin state.
     */
    private var fullScreen:Boolean = false;
    
    /**
     *  @private
     *  Holds a list of properties for the "video player state" that will 
     *  be restored when going out of fullScreen mode.
     */
    private var beforeFullScreenInfo:Object;
    
    /**
     *  @private
     *  Timer, which waits for 3 seconds by default to hide the 
     *  playback controls.  If there's interaction by the user, then 
     *  these playback controls are show again, and the timer will reset 
     *  and start the countdown.
     */
    private var fullScreenHideControlTimer:Timer;
    
    /**
     *  @private
     */
    private function fullScreenButton_clickHandler(event:MouseEvent):void
    {
        if (!fullScreen)
        {
            // check to make sure we can go into fullscreen mode
            if (!systemManager.getTopLevelRoot())
                return;
            
            var screenBounds:Rectangle = getScreenBounds();
            
            fullScreen = true;
            
            // need it to go into full screen state for the skin
            invalidateSkinState();
            
            // keep track of pauseWhenHidden b/c we will set it to false temporarily 
            // so that the video does not pause when we reparent it to the top
            // level application
            var oldPauseWhenHidden:Boolean = pauseWhenHidden;
            
            // let's get it off of our layout system so it doesn't interfere with 
            // the sizing and positioning. Then let's resize it to be 
            // the full size of our screen.  Then let's position it off-screen so
            // there are no other elements in the way.
            beforeFullScreenInfo = {parent: this.parent,
                x: this.x,
                y: this.y,
                explicitWidth: this.explicitWidth,
                explicitHeight: this.explicitHeight,
                percentWidth: this.percentWidth,
                percentHeight: this.percentHeight,
                isPopUp: this.isPopUp};
            
            pauseWhenHidden = false;
            
            if (!isPopUp)
            {
                // remove from old parent
                if (parent is IVisualElementContainer)
                {
                    var ivec:IVisualElementContainer = IVisualElementContainer(parent);
                    beforeFullScreenInfo.childIndex = ivec.getElementIndex(this);
                    ivec.removeElement(this);
                }
                else
                {
                    beforeFullScreenInfo.childIndex = parent.getChildIndex(this);
                    parent.removeChild(this);
                }
                
                // add as a popup
                PopUpManager.addPopUp(this, FlexGlobals.topLevelApplication as DisplayObject, false, null, moduleFactory);
            }
            
            // Resize the component to be the full screen of the stage.
            // Push the component at (0,0).  It should be on top of everything 
            // at this point because it was added as a popup
            setLayoutBoundsSize(screenBounds.width, screenBounds.height, true);
            // set the explicit width/height to make sure this value sticks regardless 
            // of any other code or layout passes.  Calling setLayoutBoundsSize() before hand
            // allows us to use postLayout width/height.
            // Setting explictWidth/Height sets percentWidth/Height to NaN. 
            this.explicitWidth = width;
            this.explicitHeight = height;
            setLayoutBoundsPosition(0, 0, true);
            
            // this is for video performance reasons, but sometimes the videoObject isn't there
            // if the source is null
            if (videoDisplay.videoObject)
            {
                beforeFullScreenInfo.smoothing = videoDisplay.videoObject.smoothing;
                beforeFullScreenInfo.deblocking = videoDisplay.videoObject.deblocking;
                videoDisplay.videoObject.smoothing = false;
                videoDisplay.videoObject.deblocking = 0;
            }
            
            this.validateNow();
            
            systemManager.stage.addEventListener(FullScreenEvent.FULL_SCREEN, fullScreenEventHandler);
            
            // TODO (rfrishbe): Should we make this FULL_SCREEN_INTERACTIVE if in AIR?
            systemManager.stage.displayState = StageDisplayState.FULL_SCREEN;
            
            pauseWhenHidden = oldPauseWhenHidden;

            var fullScreenHideControlsDelay:Number = getStyle("fullScreenHideControlsDelay");
            
            if (fullScreenHideControlsDelay == 0)
            {
                playerControls.visible = false;
        
                if (volumeBar)
                    volumeBar.closeDropDown(true);
            }
            else if (fullScreenHideControlsDelay < Infinity)
            {
                // start timer for detecting for mouse movements/clicks to hide the controls
                fullScreenHideControlTimer = new Timer(fullScreenHideControlsDelay, 1);
                fullScreenHideControlTimer.addEventListener(TimerEvent.TIMER_COMPLETE, 
                    fullScreenHideControlTimer_timerCompleteHandler, false, 0, true);
                
                // use stage or systemManager?
                systemManager.getSandboxRoot().addEventListener(MouseEvent.MOUSE_DOWN, resetFullScreenHideControlTimer);
                systemManager.getSandboxRoot().addEventListener(MouseEvent.MOUSE_MOVE, resetFullScreenHideControlTimer);
                systemManager.getSandboxRoot().addEventListener(MouseEvent.MOUSE_WHEEL, resetFullScreenHideControlTimer);
                
                // keyboard events don't happen when in fullScreen mode, but could be in fullScreen and interactive mode
                systemManager.getSandboxRoot().addEventListener(KeyboardEvent.KEY_DOWN, resetFullScreenHideControlTimer);
                
                fullScreenHideControlTimer.start();
            }
        }
        else
        {
            systemManager.stage.displayState = StageDisplayState.NORMAL;
        }
    }
    
    /**
     *  @private
     *  After waiting a certain time perdiod, we hide the controls if no 
     *  user-interaction has occurred on-screen.
     */
    private function fullScreenHideControlTimer_timerCompleteHandler(event:TimerEvent):void
    {
        playerControls.visible = false;
        
        if (volumeBar)
            volumeBar.closeDropDown(true);
    }
    
    /**
     *  @private
     *  Handles when mouse interaction happens, and we are in the fullscreen mode.  This 
     *  resets the fullScreenHideControlTimer.
     */
    private function resetFullScreenHideControlTimer(event:Event):void
    {
        playerControls.visible = true;
        
        if (fullScreenHideControlTimer)
        {
            fullScreenHideControlTimer.reset();
            fullScreenHideControlTimer.start();
        }
        else
        {
            fullScreenHideControlTimer = new Timer(getStyle("fullScreenHideControlsDelay"), 1);
            fullScreenHideControlTimer.addEventListener(TimerEvent.TIMER_COMPLETE, 
                fullScreenHideControlTimer_timerCompleteHandler, false, 0, true);
        }
    }
    
    /**
     *  @private
     *  Handles when coming out the full screen mode
     */
    private function fullScreenEventHandler(event:FullScreenEvent):void
    {
        // going in to full screen is handled by the 
        // fullScreenButton_clickHandler
        if (event.fullScreen)
            return;
        
        // keep track of pauseWhenHidden b/c we will set it to false temporarily 
        // so that the video does not pause when we reparent it to the top
        // level application
        exitingFullScreenPauseWhenHidden = pauseWhenHidden;
        pauseWhenHidden = false;
        
        // set the fullScreen variable back to false and remove this event listener
        fullScreen = false;
        systemManager.stage.removeEventListener(FullScreenEvent.FULL_SCREEN, fullScreenEventHandler);
        
        // remove the event listeners to hide the controls
        systemManager.getSandboxRoot().removeEventListener(MouseEvent.MOUSE_DOWN, resetFullScreenHideControlTimer);
        systemManager.getSandboxRoot().removeEventListener(MouseEvent.MOUSE_MOVE, resetFullScreenHideControlTimer);
        systemManager.getSandboxRoot().removeEventListener(MouseEvent.MOUSE_WHEEL, resetFullScreenHideControlTimer);
        systemManager.getSandboxRoot().removeEventListener(KeyboardEvent.KEY_DOWN, resetFullScreenHideControlTimer);
        
        if (fullScreenHideControlTimer)
        {
            fullScreenHideControlTimer.stop();
            fullScreenHideControlTimer = null;
        }
        
        // make the controls visible no matter what
        playerControls.visible = true;
        
        // reset it so we're re-included in the layout
        this.x = beforeFullScreenInfo.x;
        this.y = beforeFullScreenInfo.y;
        this.explicitWidth = beforeFullScreenInfo.explicitWidth;
        this.explicitHeight = beforeFullScreenInfo.explicitHeight;
        this.percentWidth = beforeFullScreenInfo.percentWidth;
        this.percentHeight = beforeFullScreenInfo.percentHeight;
        
        // sometimes there's no video object currently or there might not've been a 
        // video object when we went in to fullScreen mode.  There may be no videoObject
        // if the source hasn't been set.
        if (videoDisplay.videoObject && beforeFullScreenInfo.smoothing !== undefined)
        {
            videoDisplay.videoObject.smoothing = beforeFullScreenInfo.smoothing;
            videoDisplay.videoObject.deblocking = beforeFullScreenInfo.deblocking;
        }
        
        if (!beforeFullScreenInfo.isPopUp)
        {
            // remove from top level application:
            PopUpManager.removePopUp(this);
            
            // add back to original parent
            if (beforeFullScreenInfo.parent is IVisualElementContainer)
                beforeFullScreenInfo.parent.addElementAt(this, beforeFullScreenInfo.childIndex);
            else
                beforeFullScreenInfo.parent.addChildAt(this, beforeFullScreenInfo.childIndex);
        }

        // want to update pauseWhenHidden, but can't do it here
        // b/c the AIR window thinks it's invisible at this point
        // if we're on a Mac (a bug), so let's just defer this check
        // to commitProperties().
        if (exitingFullScreenPauseWhenHidden)
        {
            // if we need to set it back to true
            needsToUpdatePauseWhenHidden = true;
            invalidateProperties();
        }
        
        beforeFullScreenInfo = null;
        
        invalidateSkinState();
        invalidateSize();
        invalidateDisplayList();
    }
    
    /**
     *  @private
     */
    private function playButton_clickHandler(event:MouseEvent):void
    {
        if (!playing)
            play();
    }
    
    /**
     *  @private
     */
    private function pauseButton_clickHandler(event:MouseEvent):void
    {
        pause();
    }
    
    /**
     *  @private
     */
    private function stopButton_clickHandler(event:MouseEvent):void
    {
        stop();
    }
    
    /**
     *  @private
     */
    private function playPauseButton_clickHandler(event:MouseEvent):void
    {
        if (playing)
            pause();
        else
            play();
        
        // need to synch up to what we've actually got because sometimes 
        // the play() didn't actually play() because there's no source 
        // or we're in an error state
        playPauseButton.selected = playing;
    }
    
    /**
     *  @private
     */
    private function muteButton_mutedChangeHandler(event:FlexEvent):void
    {
        muted = muteButton.muted;
    }
    
    /**
     *  @private
     */
    private function volumeBar_changeHandler(event:Event):void
    {
        if (volume != volumeBar.value)
            volume = volumeBar.value;
    }
    
    /**
     *  @private
     */
    private function volumeBar_mutedChangeHandler(event:FlexEvent):void
    {
        if (muted != volumeBar.muted)
            muted = volumeBar.muted;
    }
    
    /**
     *  @private
     *  When someone is holding the scrubBar, we don't want to update the 
     *  range's value--for this time period, we'll let the user completely 
     *  control the range.
     */
    private var scrubBarMouseCaptured:Boolean;
    
    /**
     *  @private
     *  We pause the video when dragging the thumb for the scrub bar.  This 
     *  stores whether we were paused or not.
     */
    private var wasPlayingBeforeSeeking:Boolean;
    
    /**
     *  @private
     *  We are in the process of changing the timestamp
     */
    private var scrubBarChanging:Boolean;
    
    /**
     *  @private
     */
    private function scrubBar_changeStartHandler(event:Event):void
    {
        scrubBarChanging = true;
    }
    
    /**
     *  @private
     */
    private function scrubBar_thumbPressHandler(event:TrackBaseEvent):void
    {
        scrubBarMouseCaptured = true;
        if (playing)
        {
            pause();
            wasPlayingBeforeSeeking = true;
        }
    }
    
    /**
     *  @private
     */
    private function scrubBar_thumbReleaseHandler(event:TrackBaseEvent):void
    {
        scrubBarMouseCaptured = false;
        if (wasPlayingBeforeSeeking)
        {
            play();
            wasPlayingBeforeSeeking = false;
        }
    }
    
    /**
     *  @private
     */
    private function scrubBar_changeHandler(event:Event):void
    {
        seek(scrubBar.value);
    }
    
    /**
     *  @private
     */
    private function scrubBar_changeEndHandler(event:Event):void
    {      
        scrubBarChanging = false;
    }
}
}
