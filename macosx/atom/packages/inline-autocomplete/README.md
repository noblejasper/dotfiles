# Inline Autocomplete package
Originally a fork from [Autocomplete package](https://github.com/atom/autocomplete), but I made it's own repo to make reporting issues easier.

Inlines possible completions in the editor using `escape`, like Textmate's autocomplete.

![](http://imageshack.com/a/img203/3507/pmm.gif)

Looking to use a different keybinding? Copy the following to your
`~/.atom/keymap.cson` to tweak:
```
'.editor':
  'alt-space': 'inline-autocomplete:cycle'
  'shift-alt-space': 'inline-autocomplete:cycle-back'
```

You can add a keybind to confirm a selection:
```
'.inline-autocompleting':
  'space': 'inline-autocomplete:stop'
```

TODO:
 * Pull Editor's grammar to Word List
 * Figure out how to make confirmKeys customizable
 
