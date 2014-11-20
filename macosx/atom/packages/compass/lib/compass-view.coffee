{View} = require 'atom'

module.exports =
class CompassView extends View
  @content: ->
    @div class: 'compass overlay from-top', =>
      @div "The Compass package is Alive! It's ALIVE!", class: "message"

  initialize: (serializeState) ->
    atom.workspaceView.command "compass:toggle", => @toggle()

  # Returns an object that can be retrieved when package is activated
  serialize: ->

  # Tear down any state and detach
  destroy: ->
    @detach()

  toggle: ->
    console.log "CompassView was toggled!"
    if @hasParent()
      @detach()
    else
      atom.workspaceView.append(this)
