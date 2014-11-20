CompassView = require './compass-view'

module.exports =
  compassView: null

  activate: (state) ->
    @compassView = new CompassView(state.compassViewState)

  deactivate: ->
    @compassView.destroy()

  serialize: ->
    compassViewState: @compassView.serialize()
