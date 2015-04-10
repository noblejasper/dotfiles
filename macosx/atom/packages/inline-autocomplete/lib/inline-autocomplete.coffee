_ = require 'underscore-plus'
{Disposable, CompositeDisposable, Range} = require 'atom'
{$, $$} = require 'atom-space-pen-views'

module.exports =
  configDefaults:
    includeCompletionsFromAllBuffers: false
    includeGrammarKeywords: false
    regexFlags: ""
    confirmKeys: [8, 9, 13, 27, 32, 37, 38, 39, 40, 46, 48, 49, 50, 51, 57, 91, 186, 188, 190, 191, 192, 219, 220, 221, 222]

  wordRegex      : /[\w]+/g
  wordList       : null
  currentWordPos : -1     # offset for 0-bases array
  currentMatches : null
  editor         : null
  currentBuffer  : null
  editorView     : null
  
  activate: ->
    # Should I cache this or will coffeescript do it for me?
    confirmKeys = @updateConfirmKeys atom.config.get('inline-autocomplete.confirmKeys')
    atom.workspace.observeTextEditors (editor) =>
      editorView = atom.views.getView editor
      editorView.onkeydown = (e) =>
        @reset() if (e.keyCode in confirmKeys) and editorView and editorView.classList.contains('inline-autocompleting')
    
    # Clicking anywhere should reset autocomplete
    atom.views.getView(atom.workspace).onclick = (e) =>
        @reset() if @editorView? and @editorView.classList.contains('inline-autocompleting')
    atom.commands.add 'inline-autocompleting', 'inline-autocomplete:stop', (e) =>
      @reset()
    
    atom.commands.add 'atom-workspace', 'inline-autocomplete:cycle-back', (e) =>
      @toggleAutocomplete(e, -1)
    
    atom.commands.add 'atom-workspace', 'inline-autocomplete:cycle', (e) => 
      @toggleAutocomplete(e, 1)
  
  # Removes any already binded keys
  # TODO: figure out a better way to handle this for keys with modifiers
  updateConfirmKeys: (confirmKeys) =>
    for key, confirmKey of confirmKeys
      keyEvent = atom.keymap.constructor.buildKeydownEvent(String.fromCharCode(confirmKey))
      keyName = atom.keymap.constructor.prototype.keystrokeForKeyboardEvent(keyEvent)
      
      confirmKeys.splice(key, 1) if atom.keymap.findKeyBindings({'command': 'inline-autocomplete:cycle', 'keystrokes': keyName}).length > 0
      confirmKeys.splice(key, 1) if atom.keymap.findKeyBindings({'command': 'inline-autocomplete:cycle-back', 'keystrokes': keyName}).length > 0
    confirmKeys
  
  toggleAutocomplete: (e, step) -> 
    @editor = atom.workspace.getActiveTextEditor()
    if @editor?
      @currentBuffer = @editor.getBuffer()
      @editorView = atom.views.getView @editor
      cursor = @editor.getLastCursor()
      cursorPosition = @editor.getCursorBufferPosition()
      
      if @editorView and cursor.isVisible() and
      @currentBuffer.getTextInRange( Range.fromPointWithDelta(cursorPosition,0,-1)).match(/^\w$/) and
      @currentBuffer.getTextInRange( Range.fromPointWithDelta(cursorPosition,0,1)).match(/^\W*$/)
        @editorView.classList.add('inline-autocompleting')
        @cycleAutocompleteWords(step)
      else
        @reset()
        e.abortKeyBinding()
    else
      @reset()
      e.abortKeyBinding()
  
  buildWordList: ->
    wordHash = {}
    if atom.config.get('inline-autocomplete.includeCompletionsFromAllBuffers')
      buffers = atom.project.getBuffers()
    else
      buffers = [@currentBuffer]
    matches = []
    # Really goddamn ugly code here, it just strips out the match string of special characters
    # It's probably pretty damn inefficent and unreliable
    if atom.config.get('inline-autocomplete.includeGrammarKeywords')
      grammar = atom.workspace.getActiveTextEditor().getGrammar()
      if grammar and grammar.rawPatterns
        for rawPattern in grammar.rawPatterns
          if rawPattern.match
            strippedPattern = rawPattern.match.replace(/\\.{1}/g, '')
            if words = strippedPattern.match(/\w+/g)
              matches.push(word.match(@wordRegex)) if word.match(@wordRegex) for word in words
      
    matches.push(buffer.getText().match(@wordRegex)) for buffer in buffers
    wordHash[word] ?= true for word in _.flatten(matches)

    @wordList = Object.keys(wordHash).sort (word1, word2) ->
      word1.toLowerCase().localeCompare(word2.toLowerCase())
      
  replaceSelectedTextWithMatch: (match) ->
    selection = @editor.getSelection()
    startPosition = selection.getBufferRange().start
    buffer = @editor.getBuffer()
    
    selection.selectWord()
    selection.insertText(match.word, { select: false })
    # selection.insertText(match.word, { select: false, undo: 'skip' })
  
  prefixAndSuffixOfSelection: (selection) ->
    selectionRange = selection.getBufferRange()
    lineRange = [[selectionRange.start.row, 0], [selectionRange.end.row, @editor.lineLengthForBufferRow(selectionRange.end.row)]]
    [prefix, suffix] = ["", ""]

    @currentBuffer.scanInRange @wordRegex, lineRange, ({match, range, stop}) ->
      stop() if range.start.isGreaterThan(selectionRange.end)

      if range.intersectsWith(selectionRange)
        prefixOffset = selectionRange.start.column - range.start.column
        suffixOffset = selectionRange.end.column - range.end.column

        prefix = match[0][0...prefixOffset] if range.start.isLessThan(selectionRange.start)
        suffix = match[0][suffixOffset..] if range.end.isGreaterThan(selectionRange.end)

    {prefix, suffix}

  findMatchesForCurrentSelection: ->
    selection = @editor.getSelection()
    {prefix, suffix} = @prefixAndSuffixOfSelection(selection)

    if (prefix.length + suffix.length) > 0
      regex = new RegExp("^#{prefix}.+#{suffix}$", atom.config.get('inline-autocomplete.regexFlags'))
      currentWord = prefix + @editor.getSelectedText() + suffix
      for word in @wordList when regex.test(word) and word != currentWord
        {prefix, suffix, word}
    else
      {word, prefix, suffix} for word in @wordList
  
  cycleAutocompleteWords: (steps)->
    unless @wordList?
      @buildWordList()
    
    unless @currentMatches?
      @currentMatches = @findMatchesForCurrentSelection()
    
    if @currentMatches.length > 0
      if steps + @currentWordPos < 0
        @currentWordPos = @currentMatches.length + steps
      else
        @currentWordPos += steps
      @currentWordPos %= @currentMatches.length
      @replaceSelectedTextWithMatch(@currentMatches[@currentWordPos])
    # if @currentWordPos >= @currentMatches.length
    #   @reset()

  reset: ->
    @editorView.classList.remove('inline-autocompleting') if @editorView
    @wordList       = null
    @currentWordPos = -1
    @currentMatches = null
    @currentBuffer  = null
    @editorView     = null
