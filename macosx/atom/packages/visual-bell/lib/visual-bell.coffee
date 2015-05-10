module.exports =
  config:
    enabled: 
      type: 'boolean'
      default: true

  activate: ->
    atom.onDidBeep =>
      return unless atom.config.get('visual-bell.enabled')
      @addOverlay()
      setTimeout((=> @removeOverlay()), 300)

  deactivate: ->
    @removeOverlay()

  addOverlay: ->
    @removeOverlay() if @overlay
    @overlay = document.createElement 'div'
    @overlay.className = 'visual-bell'
    atom.workspace.addBottomPanel({item: @overlay})

  removeOverlay: ->
    @overlay?.remove()
    @overlay = null
