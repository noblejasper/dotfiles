provider = require './provider'

module.exports =
  config: provider.config

  activate: ->
    console.log 'activate aligner-scss'

  getProvider: -> provider
