////////////////////////////////////////////////////////////////////////////////
//
//  ADOBE SYSTEMS INCORPORATED
//  Copyright 2010 Adobe Systems Incorporated
//  All Rights Reserved.
//
//  NOTICE: Adobe permits you to use, modify, and distribute this file
//  in accordance with the terms of the license agreement accompanying it.
//
////////////////////////////////////////////////////////////////////////////////

package spark.globalization
{

import flash.globalization.LastOperationStatus;

import mx.core.mx_internal;

use namespace mx_internal;

/**
 *  The LastOperationStatus class enumerates constant values that represent the
 *  status of the most recent globalization service operation.
 *
 *  These values can be retrieved through the read-only property
 *  <code>lastOperationStatus</code> available in most globalization classes.
 *
 *  @see flash.globalization.LastOperationStatus

 *  @langversion 3.0
 *  @playerversion Flash 10.1
 *  @playerversion AIR 2.5
 *  @productversion Flex 4.55
 */
public final class LastOperationStatus
{
    include "../core/Version.as";

    //--------------------------------------------------------------------------
    //
    //  Class Constants
    //
    //--------------------------------------------------------------------------

    //--------------------------------------------------------------------------
    //  Duplicated constants from flash.globalization.LastOperationError
    //--------------------------------------------------------------------------

    /**
     *  Indicates that the last operation succeeded without any errors.
     *
     *  This status can be returned by all constructors, non-static methods,
     *  static methods and read/write properties.
     *
     *  @see flash.globalization.LastOperationStatus.NO_ERROR
     *
     *  @langversion 3.0
     *  @playerversion Flash 10.1
     *  @playerversion AIR 2.5
     *  @productversion Flex 4.5
     */
    public static const NO_ERROR:String
                            = flash.globalization.LastOperationStatus.NO_ERROR;

    /**
     *  Indicates that a fallback value was set during the most recent
     *  operation.
     *
     *  This status can be returned by constructors and methods such as
     *  <code>DateTimeFormatter.setDateTimeStyles()</code>, and when retrieving
     *  properties such as <code>CurrencyFormatter.groupingPattern</code>.
     *
     *  @see flash.globalization.LastOperationStatus.USING_FALLBACK_WARNING
     *
     *  @langversion 3.0
     *  @playerversion Flash 10.1
     *  @playerversion AIR 2.5
     *  @productversion Flex 4.5
     */
    public static const USING_FALLBACK_WARNING:String
            = flash.globalization.LastOperationStatus.USING_FALLBACK_WARNING;

    /**
     *  Indicates that an operating system default value was used during the
     *  most recent operation.
     *
     *  Class constructors can return this status.
     *
     *  @see flash.globalization.LastOperationStatus.USING_DEFAULT_WARNING
     *
     *  @langversion 3.0
     *  @playerversion Flash 10.1
     *  @playerversion AIR 2.5
     *  @productversion Flex 4.5
     */
    public static const USING_DEFAULT_WARNING:String
                = flash.globalization.LastOperationStatus.USING_DEFAULT_WARNING;

    /**
     *  Indicates that the parsing of a number failed.
     *
     *  This status can be returned by parsing methods of the formatter classes,
     *  such as <code>CurrencyFormatter.parse()</code> and
     *  <code>NumberFormatter.parseNumber()</code>.
     *  For example, if the value "12abc34" is passed as the parameter to the
     *  <code>CurrencyFormatter.parse()</code> method, the method returns "NaN"
     *  and sets the <code>lastOperationStatus</code> value to
     *  <code>LastOperationStatus.PARSE_ERROR</code>.
     *
     *  @see flash.globalization.LastOperationStatus.PARSE_ERROR
     *
     *  @langversion 3.0
     *  @playerversion Flash 10.1
     *  @playerversion AIR 2.5
     *  @productversion Flex 4.5
     */
    public static const PARSE_ERROR:String
                        = flash.globalization.LastOperationStatus.PARSE_ERROR;

    /**
     *  Indicates that the requested operation or option is not supported.
     *
     *  This status can be returned by methods such as
     *  <code>DateTimeFormatter.setDateTimePattern()</code> and when retrieving
     *  properties suce as <code>Collator.ignoreCase</code>.
     *
     *  @see flash.globalization.LastOperationStatus.UNSUPPORTED_ERROR
     *
     *  @langversion 3.0
     *  @playerversion Flash 10.1
     *  @playerversion AIR 2.5
     *  @productversion Flex 4.5
     */
    public static const UNSUPPORTED_ERROR:String
                    = flash.globalization.LastOperationStatus.UNSUPPORTED_ERROR;

    /**
     *  Indicates that the return error code is not known.
     *
     *  Any non-static method or read/write properties can return this error
     *  when the operation is not successful and the return error code is not
     *  known.
     *
     *  @see flash.globalization.LastOperationStatus.ERROR_CODE_UNKNOWN
     *
     *  @langversion 3.0
     *  @playerversion Flash 10.1
     *  @playerversion AIR 2.5
     *  @productversion Flex 4.5s
     */

    public static const ERROR_CODE_UNKNOWN:String
                = flash.globalization.LastOperationStatus.ERROR_CODE_UNKNOWN;
    /**
     *  Indicates that the pattern for formatting a number, date, or time is
     *  invalid.
     *
     *  This status is set when the user's operating system does not support the
     *  given pattern.
     *
     *  <p>For example, the following code shows the value of the
     *  <code>lastOperationStatus</code> property after an invalid "xx" pattern
     *  is used for date formatting:</p>
     *
     *  <pre>
     *  var df:DateTimeFormatter = new DateTimeFormatter();
     *  df.setStyle("locale","en_US");
     *  df.setDateTimePattern("xx");
     *  trace(df.lastOperationStatus); // "patternSyntaxError"
     *  </pre>
     *
     *  @see flash.globalization.LastOperationStatus.PATTERN_SYNTAX_ERROR
     *
     *  @langversion 3.0
     *  @playerversion Flash 10.1
     *  @playerversion AIR 2.5
     *  @productversion Flex 4.5
     */
    public static const PATTERN_SYNTAX_ERROR:String
                = flash.globalization.LastOperationStatus.PATTERN_SYNTAX_ERROR;

    /**
     *  Indicates that memory allocation has failed.
     *
     *  @see flash.globalization.LastOperationStatus.MEMORY_ALLOCATION_ERROR
     *
     *  @langversion 3.0
     *  @playerversion Flash 10.1
     *  @playerversion AIR 2.5
     *  @productversion Flex 4.5
     */
    public static const MEMORY_ALLOCATION_ERROR:String
            = flash.globalization.LastOperationStatus.MEMORY_ALLOCATION_ERROR;

    /**
     *  Indicates that an argument passed to a method was illegal.
     *
     *  <p>For example, the following code shows that an invalid argument error
     *  status is set when <code>CurrencyFormatter.grouping</code> property is
     *  set to the invalid value "3;".</p>
     *
     *  <pre>
     *  var cf:CurrencyFormatter = new CurrencyFormatter();
     *  cf.setStyle("locale","en_US");
     *  cf.groupingPattern = "3;";
     *  trace(cf.lastOperationStatus); // "illegalArgumentError"
     *  </pre>
     *
     *  @see flash.globalization.LastOperationStatus.ILLEGAL_ARGUMENT_ERROR
     *
     *  @langversion 3.0
     *  @playerversion Flash 10.1
     *  @playerversion AIR 2.5
     *  @productversion Flex 4.5
     */
    public static const ILLEGAL_ARGUMENT_ERROR:String
            = flash.globalization.LastOperationStatus.ILLEGAL_ARGUMENT_ERROR;

    /**
     *  Indicates that given buffer is not enough to hold the result.
     *
     *  @see flash.globalization.LastOperationStatus.BUFFER_OVERFLOW_ERROR
     *
     *  @langversion 3.0
     *  @playerversion Flash 10.1
     *  @playerversion AIR 2.5
     *  @productversion Flex 4.5
     */

    public static const BUFFER_OVERFLOW_ERROR:String
                = flash.globalization.LastOperationStatus.BUFFER_OVERFLOW_ERROR;

    /**
     *  Indicates that a given attribute value is out of the expected range.
     *
     *  <p>The following example shows that setting the
     *  <code>NumberFormatter.negativeNumberFormat</code> property to an
     *  out-of-range value results in an invalid attribute value status.</p>
     *
     *  <pre>
     *  var nf:NumberFormatter = new NumberFormatter();
     *  nf.setStyle("locale","en_US");
     *  nf.negativeNumberFormat = 9;
     *  nf.lastOperationStatus; // "invalidAttrValue"
     *  </pre>
     *
     *  @see flash.globalization.LastOperationStatus.INVALID_ATTR_VALUE
     *
     *  @langversion 3.0
     *  @playerversion Flash 10.1
     *  @playerversion AIR 2.5
     *  @productversion Flex 4.5
     */
    public static const INVALID_ATTR_VALUE:String
                = flash.globalization.LastOperationStatus.INVALID_ATTR_VALUE;

    /**
     *  Indicates that an operation resulted a value that exceeds a specified
     *  numeric type.
     *
     *  @see flash.globalization.LastOperationStatus.NUMBER_OVERFLOW_ERROR
     *
     *  @langversion 3.0
     *  @playerversion Flash 10.1
     *  @playerversion AIR 2.5
     *  @productversion Flex 4.5
     */
    public static const NUMBER_OVERFLOW_ERROR:String
                = flash.globalization.LastOperationStatus.NUMBER_OVERFLOW_ERROR;

    /**
     *  Indicates that invalid Unicode value was found.
     *
     *  @see flash.globalization.LastOperationStatus.INVALID_CHAR_FOUND
     *
     *  @langversion 3.0
     *  @playerversion Flash 10.1
     *  @playerversion AIR 2.5
     *  @productversion Flex 4.5
     */
    public static const INVALID_CHAR_FOUND:String
                = flash.globalization.LastOperationStatus.INVALID_CHAR_FOUND;

    /**
     *  Indicates that a truncated Unicode character value was found.
     *
     *  @see flash.globalization.LastOperationStatus.TRUNCATED_CHAR_FOUND
     *
     *  @langversion 3.0
     *  @playerversion Flash 10.1
     *  @playerversion AIR 2.5
     *  @productversion Flex 4.5
     */
    public static const TRUNCATED_CHAR_FOUND:String
                = flash.globalization.LastOperationStatus.TRUNCATED_CHAR_FOUND;

    /**
     *  Indicates that an iterator went out of range or an invalid parameter was
     *  specified for month, day, or time.
     *
     *  @see flash.globalization.LastOperationStatus.INDEX_OUT_OF_BOUNDS_ERROR
     *
     *  @langversion 3.0
     *  @playerversion Flash 10.1
     *  @playerversion AIR 2.5
     *  @productversion Flex 4.5
     */
    public static const INDEX_OUT_OF_BOUNDS_ERROR:String
            = flash.globalization.LastOperationStatus.INDEX_OUT_OF_BOUNDS_ERROR;

    /**
     *  Indicates that an underlying platform API failed for an operation.
     *
     *  @see flash.globalization.LastOperationStatus.PLATFORM_API_FAILED
     *
     *  @langversion 3.0
     *  @playerversion Flash 10.1
     *  @playerversion AIR 2.5
     *  @productversion Flex 4.5
     */
    public static const PLATFORM_API_FAILED:String
                = flash.globalization.LastOperationStatus.PLATFORM_API_FAILED;

    /**
     *  Indicates that an unexpected token was detected in a Locale ID string.
     *
     *  <p>For example, the following code shows the value of the
     *  <code>lastOperationStatus</code> property after an incomplete string is
     *  used when requesting a locale ID.
     *  As a result the <code>lastOperationStatus</code> property is set to the
     *  value <code>UNEXPECTED_TOKEN</code> after a call to the
     *  <code>LocaleID.getKeysAndValues()</code> method.</p>
     *
     *  <pre>
     *  var locale:flash.globalization.LocaleID = new flash.globalization.LocaleID("en-US&#64;Collation");
     *  var kav:Object = locale.getKeysAndValues();
     *  trace(locale.lastOperationStatus); // "unexpectedToken"
     *  </pre>
     *
     *  @see flash.globalization.LastOperationStatus.UNEXPECTED_TOKEN
     *
     *  @langversion 3.0
     *  @playerversion Flash 10.1
     *  @playerversion AIR 2.5
     *  @productversion Flex 4.5
     */
    public static const UNEXPECTED_TOKEN:String
                    = flash.globalization.LastOperationStatus.UNEXPECTED_TOKEN;

    //--------------------------------------------------------------------------
    //  Additional constants besides constants from
    //  flash.globalization.LastOperationError
    //--------------------------------------------------------------------------

    /**
     *  Indicates that <code>Locale</code> is not defined.
     *
     *  @langversion 3.0
     *  @playerversion Flash 10.1
     *  @playerversion AIR 2.5
     *  @productversion Flex 4.5
     */
    public static const LOCALE_UNDEFINED_ERROR:String = "localeUndefinedError";

    //--------------------------------------------------------------------------
    //
    //  Methods
    //
    //--------------------------------------------------------------------------

    /**
    *  @private
    *  Check if given <code>lastOperationStatus</code> is a error (not a warning).
    *
    *  A fatal error means errors other than no-error and warnings.
    */
    mx_internal static function isError(lastOperationStatus:String):Boolean
    {
        switch (lastOperationStatus)
        {
            case NO_ERROR:
            case USING_FALLBACK_WARNING:
            case USING_DEFAULT_WARNING:
                return false;
        }
        return true;
    }
}
}
