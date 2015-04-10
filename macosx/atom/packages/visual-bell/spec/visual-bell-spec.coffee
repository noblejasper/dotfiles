VisualBell = require '../lib/visual-bell'

describe "VisualBell", ->
  [workspaceElement, subscribe] = []
  beforeEach ->
    workspaceElement = atom.views.getView(atom.workspace)

    waitsForPromise ->
      atom.packages.activatePackage("visual-bell")
      
  afterEach ->
    subscribe.dispose()

  describe "when visual bells are enabled (default)", ->
    it "appends div.visual-bell to body on 'beep' event", ->
      subscribe = atom.onDidBeep =>
        expect(workspaceElement.querySelector('.visual-bell')).toExist()
        expect(atom.workspace.panelForItem(workspaceElement.querySelector('.visual-bell')).isVisible()).toBeTruthy()

      atom.beep()

    it "does not ever create two overlays", ->
      subscribe = atom.onDidBeep =>
        expect(workspaceElement.querySelectorAll('.visual-bell')).toHaveLength 1

      atom.beep()
      atom.beep()

  describe "when visual bells are disabled", ->
    it "does not append div.visual-bell on 'beep' event", ->
      subscribe = atom.onDidBeep =>
        expect(workspaceElement.querySelector('.visual-bell')).not.toExist()
      
      atom.config.set('visual-bell.enabled', false)
      atom.beep()
