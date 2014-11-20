var path = require("path");
var fs = require("fs");

function getConfigFileName(baseFileName, configFileName) {
    var baseDir = path.dirname(baseFileName);

    if (fs.existsSync(baseDir + path.sep + configFileName)) {
        return baseDir + path.sep + configFileName;
    }

    if (baseDir.length === path.dirname(baseDir).length) {
        return null;
    }

    return getConfigFileName(baseDir, configFileName);
}

function makeFormatCodeOptions(fileName, options) {
    var configFileName = getConfigFileName(path.resolve(fileName), "tsfmt.json");
    if (!configFileName) {
        return options;
    }

    var config = JSON.parse(fs.readFileSync(configFileName, "utf-8"));
    if (typeof config.insertSpaceAfterCommaDelimiter === "boolean") {
        options.InsertSpaceAfterCommaDelimiter = config.insertSpaceAfterCommaDelimiter;
    }
    if (typeof config.insertSpaceAfterSemicolonInForStatements === "boolean") {
        options.InsertSpaceAfterSemicolonInForStatements = config.insertSpaceAfterSemicolonInForStatements;
    }
    if (typeof config.insertSpaceBeforeAndAfterBinaryOperators === "boolean") {
        options.InsertSpaceBeforeAndAfterBinaryOperators = config.insertSpaceBeforeAndAfterBinaryOperators;
    }
    if (typeof config.insertSpaceAfterKeywordsInControlFlowStatements === "boolean") {
        options.InsertSpaceAfterKeywordsInControlFlowStatements = config.insertSpaceAfterKeywordsInControlFlowStatements;
    }
    if (typeof config.insertSpaceAfterFunctionKeywordForAnonymousFunctions === "boolean") {
        options.InsertSpaceAfterFunctionKeywordForAnonymousFunctions = config.insertSpaceAfterFunctionKeywordForAnonymousFunctions;
    }
    if (typeof config.insertSpaceAfterOpeningAndBeforeClosingNonemptyParenthesis === "boolean") {
        options.InsertSpaceAfterOpeningAndBeforeClosingNonemptyParenthesis = config.insertSpaceAfterOpeningAndBeforeClosingNonemptyParenthesis;
    }
    if (typeof config.placeOpenBraceOnNewLineForFunctions === "boolean") {
        options.PlaceOpenBraceOnNewLineForFunctions = config.placeOpenBraceOnNewLineForFunctions;
    }
    if (typeof config.placeOpenBraceOnNewLineForControlBlocks === "boolean") {
        options.PlaceOpenBraceOnNewLineForControlBlocks = config.placeOpenBraceOnNewLineForControlBlocks;
    }
    if (typeof config.indentSize === "number") {
        options.IndentSize = config.indentSize;
    }
    if (typeof config.tabSize === "number") {
        options.TabSize = config.tabSize;
    }
    if (typeof config.newLineCharacter === "string") {
        options.NewLineCharacter = config.newLineCharacter;
    }
    if (typeof config.convertTabsToSpaces === "boolean") {
        options.ConvertTabsToSpaces = config.convertTabsToSpaces;
    }

    return options;
}
exports.makeFormatCodeOptions = makeFormatCodeOptions;
