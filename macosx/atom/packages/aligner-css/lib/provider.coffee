module.exports =
  selector: ['.source.css', '.source.html', '.source.css.less']
  id: 'aligner-css' # package name
  config:
    ':-alignment':
      title: 'Padding for :'
      description: 'Pad left or right of the character'
      type: 'string'
      default: 'right'
    ':-leftSpace':
      title: 'Left space for :'
      description: 'Add 1 whitespace to the left'
      type: 'boolean'
      default: false
    ':-rightSpace':
      title: 'Right space for :'
      description: 'Add 1 whitespace to the right'
      type: 'boolean'
      default: true
    ':-scope':
      title: 'Character scope'
      description: 'Scope string to match'
      type: 'string'
      default: 'key-value'
