# Atom Backbone.js Package

Atom Backbone provides Backbone shortcuts for CoffeeScript, JavaScript and HTML

Key features:

  - HTML micro-template language, autocompletion and snippets
  - Backbone.LayoutManager autocompletion and snippets
  - CoffeeScript and JavaScript support (Coming Soon: TypeScript)
  - Programmatic Helpers and Common Patterns snippets
  - AMD and CommonJS wrapper snippets

![Atom Backbone](https://dl.dropboxusercontent.com/u/20947008/webbox/atom/atom-backbone.gif)

## Autocompletion Support

Backbone plugin have autocompletion support all of the *Backbone* methods and classes.
It also includes *Underscore or Lo-dash* autocompletion.

## Base Snippet Activators

  - `model`: Creates model snippet
  - `view`: Creates view snippet
  - `collection`: Creates collection snippet
  - `router`: Creates router snippet
  - `layout`: Creates Backbone LayoutManager's layout

## Programmatic Helpers, Patterns

  - `modelview`: Creates model view helper snippet
  - `collectionview`: Creates collection view helper snippet
  - `super`: Creates super method caller
  - `template`: Creates `_.template` snippet
  - `history`: Generates `Bacbone.Histroy.start` function
  - `layoutconfig`: Generates Backbone LayoutManager configuration scope

## Template Helpers

All template helpers works in HTML scope. They all have "%" prefix.

  - `%`: Generates `<% %>`
  - `%=`: Generates `<%= %>`
  - `%if`: Generates if helper in template with template syntax.
  - `%ifels`: Generates if-else helper in template with template syntax
  - `%for`: Generates for loop in template syntax
  - `%in`: Geneates for-in loop in template syntax
  - `%each`: Genearates `_.each` loop in template syntax
  - `%template`: Generates script template tag

## AMD Helpers

`def` prefix generates AMD wrapper. It puts the keyword to the body and
you should push `tab` key after body.

  - `defmodel`: Generates AMD for model class
  - `defview`: Generates AMD for view class
  - `defcollection`: Generates AMD for collection class
  - `defrouter`: Generates AMD for router class
  - `deflayout`: Generates AMD for layout class

## CommonJS Helpers

`exp` prefix generates CommonJS wrapper (you can remember with "export"). It puts the keyword to the body and
you should push `tab` key after body.

  - `expmodel`: Generates CommonJS module for model class
  - `expview`: Generates CommonJS module for view class
  - `expcollection`: Generates CommonJS module for collection class
  - `exprouter`: Generates CommonJS module for router class
  - `explayout`: Generates CommonJS module for layout class
