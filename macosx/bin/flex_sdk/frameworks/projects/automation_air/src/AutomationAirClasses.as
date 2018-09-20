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



package
{

/**
 *  @private
 *  In some projects, this class is used to link additional classes
 *  into the SWC beyond those that are found by dependency analysis
 *  starting from the classes specified in manifest.xml.
 *  This project has no manifest file (because there are no MXML tags
 *  corresponding to any classes in it) so all the classes linked into
 *  the SWC are found by a dependency analysis starting from the classes
 *  listed here.
 */
internal class AutomationAirClasses
{
 
	import mx.automation.air.AirDragManagerAutomationHandler;AirDragManagerAutomationHandler;  
	import mx.automation.air.AirFunctionsHelper;AirFunctionsHelper;  
	import mx.automation.delegates.containers.WindowedApplicationAutomationImpl;WindowedApplicationAutomationImpl;
	import mx.automation.delegates.containers.WindowsAutomationImpl;WindowsAutomationImpl;
	import mx.automation.delegates.controls.fileSystemClasses.FileSystemDataGridNameColumnRendererAutomationImpl;FileSystemDataGridNameColumnRendererAutomationImpl;
	import mx.automation.delegates.controls.FileSystemTreeAutomationImpl;FileSystemTreeAutomationImpl;
	import mx.automation.delegates.controls.FlexNativeMenuAutomationImpl;FlexNativeMenuAutomationImpl;
	//Maintain alphabetical order
}

}
