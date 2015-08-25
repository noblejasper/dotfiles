{SelectListView} = require 'atom-space-pen-views'

module.exports =
class CompassView extends SelectListView
  initialize: ->
    super()
    @addClass('compass');
    atom.commands.add 'atom-workspace', "compass:toggle", => @toggle()

  # Tear down any state and detach
  destroy: ->
    @detach()

  cancelled: ->
    @hide()

  toggle: ->
    if @panel?.isVisible()
      @cancel()
    else
      @show()

  show: ->
    @panel ?= atom.workspace.addModalPanel(item: this)
    @panel.show()
    @storeFocusedElement()
