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
    var configFileName = getConfigFileName(path.resolve(fileName), "tslint.json");
    if (!configFileName) {
        return options;
    }

    var config = JSON.parse(fs.readFileSync(configFileName, "utf-8"));
    if (!config.rules) {
        return options;
    }
    if (config.rules.indent && config.rules.indent[0]) {
        options.IndentSize = config.rules.indent[1];
    }
    if (config.rules.whitespace && config.rules.whitespace[0]) {
        for (var p in config.rules.whitespace) {
            var value = config.rules.whitespace[p];
            if (value === "check-branch") {
                options.InsertSpaceAfterKeywordsInControlFlowStatements = true;
            } else if (value === "check-decl") {
            } else if (value === "check-operator") {
                options.InsertSpaceBeforeAndAfterBinaryOperators = true;
            } else if (value === "check-separator") {
                options.InsertSpaceAfterCommaDelimiter = true;
                options.InsertSpaceAfterSemicolonInForStatements = true;
            } else if (value === "check-type") {
            }
        }
    }

    return options;
}
exports.makeFormatCodeOptions = makeFormatCodeOptions;
