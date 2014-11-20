linterPath = atom.packages.getLoadedPackage("linter").path
console.log(linterPath)
Linter = require "#{linterPath}/lib/linter"

class LinterPyflakes extends Linter
  @syntax: ['source.python']

  cmd: 'pyflakes'

  executablePath: null

  linterName: 'pyflakes'

  # A regex pattern used to extract information from the executable's output.
  regex:
    ':(?<line>\\d+): (?<message>.*?)\n'

  constructor: (editor)->
    super(editor)

    atom.config.observe 'linter-python-pyflakes.pyflakesDirToExecutable', =>
      @executablePath = atom.config.get 'linter-python-pyflakes.pyflakesDirToExecutable'

  destroy: ->
    atom.config.unobserve 'linter-python-pyflakes.pyflakesDirToExecutable'

module.exports = LinterPyflakes
