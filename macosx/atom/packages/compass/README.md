# Compass Framework Snippets for Atom Editor

So far, CSS3, Layout and Typography are done [See TODOs](#todo)

[See Tab Triggers](#triggers)

- If one word, type the property name, for example:
	- appearance => `appearance`
	- filter => `filter`
- or the first initial of each word in the property name (unless it would cause a duplicate tab):
	- box-shadow => `bsh`
	- word-break => `wb`

- In some cases I have abbreviated any normally abbreviated words, for example:
	- background => bg `bgsize`
	- border-radius => br `br<position(s) first initial>` or `radius<position(s) first initial>`
	- columns => cols `cols`
	- column => col `colwidth`
	- horizontal and vertical => `h` and `v` respectively

<a name="triggers"></a>
##CSS3

### [appearance] Appearance
```
@include appearance();
```

### [ff] Font Face
```
@include font-face();
```

### [filter] Filter
```
@include filter();
```



####Background

### [bgc] Background Clip
```
@include background-clip();
```

### [bgo] Background Origin
```
@include background-origin();
```

### [bgs] Background Size
```
@include background-size();
```



####Border Radius

### [br] Border Radius
```
@include border-radius();
```

### [brb] Border Radius Bottom
```
@include border-bottom-radius();
```

### [brbl] Border Radius Bottom Left
```
@include border-bottom-left-radius();
```

### [brbr] Border Radius Bottom Right
```
@include border-bottom-right-radius();
```

### [brc] Border Corner Radius
```
@include border-corner-radius();
```

### [brl] Border Radius Left
```
@include border-left-radius();
```

### [brr] Border Radius Right
```
@include border-right-radius();
```

### [brt] Border Radius Top
```
@include border-top-radius();
```

### [brtl] Border Radius Top Left
```
@include border-top-left-radius();
```

### [brtr] Border Radius Top Right
```
@include border-top-right-radius();
```



####Box Shadow

### [bsh] Box Shadow
```
@include box-shadow();
```

### [bshs] Box Shadow Single
```
@include single-box-shadow();
```



####Box

### [ba] Box Align
```
@include box-align();
```

### [bd] Box Direction
```
@include box-direction();
```

### [bf] Box Flex
```
@include box-flex();
```

### [bfg] Box Flex Group
```
@include box-flex-group();
```

### [bl] Box Lines
```
@include box-lines();
```

### [bo] Box Orient
```
@include box-orient();
```

### [bog] Box Ordinal Group
```
@include box-ordinal-group();
```

### [bp] Box Pack
```
@include box-pack();
```

### [db] Display Box
```
@include display-box;
```



####Columns

### [cb] Column Break
```
@include column-break();
```

### [cba] Column Break After
```
@include column-break-after();
```

### [cbb] Column Break Before
```
@include column-break-before();
```

### [cbi] Column Break Inside
```
@include column-break-inside();
```

### [colc] Column Count
```
@include column-count();
```

### [colg] Column Gap
```
@include column-gap();
```

### [colr] Column Rule
```
@include column-rule();
```

### [colrc] Column Rule Color
```
@include column-rule-color();
```

### [colrs] Column Rule Style
```
@include column-rule-style();
```

### [colrw] Column Rule Width
```
@include column-rule-width();
```
### [cols] Columns
```
@include columns();
```

### [colw] Column Width
```
@include column-width();
```


####CSS Regions

### [flowf] CSS Regions Flow From
```
@include flow-from();
```

### [flowi] CSS Regions Flow Into
```
@include flow-into();
```



####Hyphenation

### [hyp] Hyphens
```
@include hyphens();
```

### [wb] Wordbreak
```
@include word-break();
```


####Images

### [bg] Background
```
@include background();
```

### [bgf] Background w/ Fallback
```
@include background-with-css2-fallback();
```

### [bgimg] Background Image
```
@include background-image();
```

### [bimg] Border Image
```
@include border-image();
```
### [content] Content
```
@include content();
```
### [fg] Filter Gradient
```
@include filter-gradient();
```
### [ls] List Style
```
@include list-style();
```
### [lsimg] List Style Image
```
@include list-style-image();
```



####Inline Block

### [ib] Inline Block
```
@include inline-block();
```



####Opacity

### [opc] Opacity
```
@include opacity();
```

### [opq] Opaque
```
@include opaque;
```

### [trp] Transparent
```
@include transparent;
```


####Text Shadow
### [tss] Text Shadow Single
```
@include single-text-shadow();
```

### [ts] Text Shadow
```
@include text-shadow();
```



####Transform

### [ao] Apply Origin
```
@include apply-origin();
```

### [bv] Backface Visibility
```
@include backface-visibility();
```

### [ctf] Create Transform
```
@include create-transform();
```

### [perspective] Perspective
```
@include perspective();
```

### [po] Perspective Origin
```
@include perspective-origin();
```

### [r3d] Rotate 3d
```
@include rotate3d();
```

### [rotate] Rotate
```
@include rotate();
```

### [rx] Rotate X
```
@include rotateX();
```

### [ry] Rotate Y
```
@include rotateY();
```

### [rz] Rotate Z
```
@include rotateZ();
```


### [sc3d] Scale 3d
```
@include scale3d();
```

### [scale] Scale
```
@include scale();
```

### [scx] Scale X
```
@include scaleX();
```

### [scy] Scale Y
```
@include scaleY();
```

### [scz] Scale Z
```
@include scaleZ();
```

### [skew] Skew
```
@include skew();
```

### [skx] Skew X
```
@include skewX();
```

### [sky] Skew Y
```
@include skewY();
```

### [stf] Simple Transform
```
@include simple-transform();
```

### [tf2d] Transform 2d
```
@include transform2d();
```

### [tf3d] Transform 3d
```
@include transform3d();
```

### [tfo] Transform Origin
```
@include transform-origin();
```

### [tfs] Transform Style
```
@include transform-style();
```

### [tl3d] Translate 3d
```
@include translate3d();
```

### [tlx] Translate X
```
@include translateX();
```

### [tly] Translate Y
```
@include translateY();
```

### [tlz] Translate Z
```
@include translateZ();
```

### [transform] Transform
```
@include transform();
```

### [translate] Translate
```
@include translate();
```



####Transition

### [ttd] Transition Duration
```
@include transition-duration();
```

### [ttf] Transition Time Function
```
@include transition-time-function();
```

### [ttp] Transition Property
```
@include transition-property();
```

### [tts] Single Transition
```
@include single-transition();
```

### [transition] Transition
```
@include transition();
```



##Layout

####Grid Background

### [bbg] Baseline Grid Background
```
@include baseline-grid-background();
```

### [colgbg] Column Grid Background
```
@include column-grid-background();
```

### [gbg] Grid Background
```
@include grid-background();
```

### [sf] or [stickyfooter] Sticky Footer
```
@include sticky-footer();
```

### [stretch] Stretch
```
@include stretch();
```

### [stretchx] Stretch X
```
@include stretch-x();
```

### [stretchy] Stretch Y
```
@include stretch-y();
```



##Typography

####Links

### [hvl] Hover Link
```
@include hover-link;
```

### [lc] Link Colors
```
@include link-colors();
```

### [ul] Unstyled Link
```
@include unstyled-link;
```


####List

#####Bullets

### [nob] No Bullet
```
@include no-bullet;
```

### [nobs] No Bullets
```
@include no-bullets;
```

### [pbs] Pretty Bullets
```
@include pretty-bullets();
```



#####Horizontal Lists
### [hl] Horizontal List
```
@include horizontal-list();
```

### [hlc] Horizontal List Container
```
@include horizontal-list-container;
```

### [hli] Horizontal List Item
```
@include horizontal-list-item();
```



#####Inline Lists
### [il] Inline List
```
@include inline-list;
```

### [cd] Comma Delimited
```
@include comma-delimited-list;
```

### [dl] Delimited List
```
@include delimited-list();
```


#####Inline Block Lists
### [ibl] Inline Block List
```
@include inline-block-list();
```

### [iblc] Inline Block List Container
```
@include inline-block-list-container;
```

### [ibli] Inline Block List Item
```
@include inline-block-list-item();
```


####Text

### [ell] Ellipsis
```
@include ellipsis;
```

### [fw] Force Wrap
```
@include force-wrap;
```

### [now] No Wrap
```
@include nowrap;
```

### [nowrap] No Wrap
```
@include nowrap;
```


##### Text Replacement

### [ht] Hide Text
```
@include hide-text();
```

### [rt] Replace Text
```
@include replace-text();
```

### [rtd] Replace Text w/ Dimensions
```
@include replace-text-with-dimensions();
```

### [sqt] Squish Text
```
@include squish-text;
```



#### Vertical Rhythm

### [afst] Adjust Font Size To
```
@include adjust-font-size-to();
```
### [alt] Adjust Leading To
```
@include adjust-leading-to();
```

### [asrb] Apply Side Rhythm Border
```
@include apply-side-rhythm-border();
```

### [da] Debug Vertical Alignment
```
@include debug-vertical-alignment();
```

### [eb] Establish Baseline
```
@include establish-baseline();
```

### [hb] H Borders
```
@include h-borders();
```

### [lb] Leading Border
```
@include leading-border();
```

### [leader] Leader
```
@include leader();
```

### [ml] Margin Leader
```
@include margin-leader();
```

### [mt] Margin Trailer
```
@include margin-trailer();
```

### [pl] Padding Leader
```
@include padding-leader();
```

### [pt] Padding Trailer
```
@include padding-trailer();
```

### [rhythm] Rhythm
```
@include rythym();
```

### [rb] Rhythm Borders
```
@include rhythm-borders();
```

### [tb] Trailing Border
```
@include trailing-border();
```

### [trailer] Trailer
```
@include trailer();
```


####Compass Colors

### [debug] Sass Debug
```
@debug;
```

### [defmix] Sass Create Mixin
```
@mixin mixinName(args) {}
```

### [each] Sass each
```
@each item in items {}
```

### [ext] Sass Extend
```
@extend;
```

### [for] Sass for
```
@for item from items {}
```

### [for] Sass for
```
@for item from items {}
```

### [if] Sass If
```
@if condition {}
```

### [ifelse] Sass If Else
```
@if condition {}
```

### [media] Sass Media
```
@media
```
### [mix] Sass Include Mixin
```
@include mixinName();
```

### [warn] Sass Warn
```
@warn;
```

### [while] Sass While
```
@while condition {}
```

### [lighten] Compass Color Lighten
```
lighten();
```

### [rgba] Compass Color RGBA
```
rgba();
```



Contributions are greatly appreciated. Please fork this repository and open a
pull request to fix something, add snippets or wrap up any TODO.

<a name="todo"></a>
### TODO
- Add Compass: ~~layout~~, ~~typography~~, utilities
- Move Sass snippets to Atoms package via PR (planned for this week)
- Add all Compass functions http://compass-style.org/index/functions/
