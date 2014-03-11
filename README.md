# texatl

This is a simple texture atlas generator.  It will generate atlases
for the following:

* Fonts, including font metrics, using
  [cl-freetype2](https://github.com/rpav/cl-freetype2) and
  [cl-cairo2](https://github.com/rpav/cl-cairo2)
* Sprites, including metrics (texture position and frames) and sprite
  names, from PNG files.

```lisp
;; This will produce a 128x128 png file with a rendition of the
;; specified characters at 18 points and 72 dpi.

(make-font-atlas-files "times.png" "times.met" 128 128
                       "/user/share/fonts/corefonts/times.ttf"
                       18
                       :dpi 72
                       :string "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789.,;:?!@#$%^&*()-_<>'\"$[] ")
```

[Example output PNG.](http://ogmo.mephle.net/times.png)

```lisp
;; This will produce a 96x96 png file as seen below.

(make-sprite-atlas-files "sprites.png" "sprites.met" 96 96
                         (directory "*.png"))
```

[Example output PNG](http://ogmo.mephle.net/sprites.png), generated
for an upcoming game using [cl-sdl2](https://github.com/lispgames/cl-sdl2).

## TEXATL vs TEXATL.CL

There are now two different systems and packages:

* `:texatl.cl`, via the system `texatl-client`, is a "client" library,
  the pure-CL functions and dependencies necessary to render fonts,
  assuming you have loaded them.

* `:texatl`, via the system `texatl`, has foreign dependencies, but is
  able to generate textures on the fly.

## Fonts

### make-font-atlas

The primary function for generating an atlas is `TEXATL:MAKE-FONT-ATLAS`:

```lisp
(make-font-atlas WIDTH HEIGHT FONT-NAME POINT-SIZE
                 &key (DPI 72) (STRING *default-characters*))

=> (values SURFACE TEXATL-FONT)
```

`Surface` is a cairo *image* surface of the specified size.

`Texatl-font` is a class with the following slots:

* `face-metrics`: This provides the following 4 metrics which may be
  used for layout:
    - `max-ascender`: The max distance from the baseline to the top of
      any glyph.  This may be used with `x` and `top` to determine the
      baseline.
    - `max-descender`: The max distance from the baseline to the lowest
      point of any glyph.  This is expressed as a negative number.
    - `height`: This is the distance *between baselines* font.
      (Generally this is rather small and should be treated as a
      minimum rather than a practical value.)
    - `max-advance`: The maximum width of any glyph.
* `glyph-index`: A hash table of character-to-index.  Each index is
  the element of the array of glyph metrics for that character.
* `glyph-metrics`: An array of metrics vectors.  Each element in this
  array corresponds to one character.  The elements of each metrics
  vector are as follows:
    - `x`, `y`: The first two elements are the pixel-offset of the *top
      left* of the glyph in the texture.  You will likely use these
      directly as `U,V` when rendering.  **Not baseline-relative.**
    - `width`, `height`: The width and height of the glyph.  You will
      likely use these as `U,V` and for vertices.  **Not
      baseline-relative.**
    - `advance`: The number of pixels to move forward to the next
      glyph, *not* including kerning (which depends on the previous
      glyph).
    - `left`: The *left* of the glyph, relative to its `x`.  You will
      want to *subtract* this from `x` when placing the glyph.
    - `top`: The *top* of a glyph, relative to its `y`.  You will want
      to *subtract* this from `max-ascender` when placing the glyph.
* `glyph-kerning`: This is an alist of `((A . B) . K)`, where `A` and
  `B` are characters, and `K` is the kerning value.  When placing
  characters:
    - Keep a running tally of the character offset, `cx`, incrementing
      this by `advance + K` after every glyph.
    - You may compute the horizontal position of character `B` by
      `cx + left + K`, when `B` follows `A`.
    - If there is no `(A . B)`, or for the initial character, `K=0`.
    - Note that the kerning for two characters "XY" may differ from
      "YX".

This may all seem rather complicated, but generally, it is not:

* Write out characters using the `cx + left + K` formula.
* The vertical position of a glyph should be `max-ascender - top`.
* For multiple lines, also add `line * (height + some-value)`.
* You should round values for pixel alignment.

See
[sdl2-manual.lisp](https://github.com/rpav/texatl/blob/master/examples/sdl2-manual.lisp)
for an example of doing this by hand.  This may be desirable for
complex font placement situations.

However, you can now use the `DO-TEXATL-STRING` macro to simplify this
process for simple 2D static string placement.  See below for details.

### make-font-atlas-files

```lisp
(make-font-atlas-files PNG-FILENAME METRICS-FILENAME
                       WIDTH HEIGHT FONT-NAME POINT-SIZE
                       &key (DPI 72) (STRING *default-characters*))
```

This function has the same conventions as `MAKE-FONT-ATLAS`, but you
may specify filenames for a PNG and metrics file.  The surface and
metrics are not returned, but instead written to these files.

The metrics file contains the same values as returned by
`MAKE-FONT-ATLAS`, written in the CONSPACK format.  You may read these
in sequence, and obtain the same values in the same format.

### do-texatl-string

```lisp
(do-texatl-string (STRING X0 Y0 X1 Y1 U0 V0 U1 V1
                   &key (tex-width 1) (tex-height 1)) TEXATL-FONT
    &body body)
```

This macro is provided for simplifying the placement of simple 2D
strings.  It provides no complex layout, line wrapping, etc, but it
does provide a way to place simple strings without a lot of work.
See
[sdl2-simple.lisp](https://github.com/rpav/texatl/blob/master/examples/sdl2-simple.lisp)
for an example.

## Sprites

Sprites are much simpler, though they have similar functions:

```lisp
(make-sprite-atlas WIDTH HEIGHT FILE-LIST)

=> (values SURFACE TEXATL-SPRITESHEET)
```

`Surface` is a cairo *image* surface of the specified size.

`Texatl-spritesheet` is a class which you can access with the
following functions:

```lisp
(sprite SPRITESHEET NAME FRAME)

=> #(X0 Y0 X1 Y1)
```

This returns the texture coordinates (in *pixels*) for the sprite
named `NAME`, for frame `FRAME`.  See *Sprite Naming* below for
details on how sprite names work.

For easy binding, there is `WITH-SPRITE`:

```lisp
(with-sprite (x0 y0 x1 y1) NAME FRAME SPRITESHEET
  &body)
```

This binds the specified variables to texture coordinates, given
`NAME`, `FRAME`, and `SPRITESHEET` (all evaluated).

Additionally, you can find how many frames a given sprite has:

```lisp
(frame-count SPRITESHEET NAME)

=> frame-count
```

### make-sprite-altas-files

Much like `MAKE-FONT-ATLAS-FILES`, this will write a PNG file and a
CONSPACK file containing the metrics for you:

```lisp
(make-sprite-atlas-files PNG-FILENAME METRICS-FILENAME WIDTH HEIGHT FILES)
```

### Sprite Naming

Sprites have names which are a *list of keywords*.  For instance, you
may have a sprite named `(:HERO :FRONT :SHOOTING)`.  In the example
spritesheet at the top, sprites have names like `(:WIZ :SIDE :WALK)`
or `(:KNIGHT :FRONT :STAB)`.

Sprites also have one or more *frames*, and given a *name* and a
*frame*, you can retrieve the specific texture coordinates with
`TEXATL.CL:SPRITE`.

Sprite *filenames* are important, because they determine sprite names
and frames.  For instance:

* `wiz:side:walk:1.png` => `(:WIZ :SIDE :WALK)` frame 1
* `knight:front:stab:0.png` => `(:KNIGHT :FRONT :STAB)` frame 0

Note that frames are numbered from *zero*.  Sprite name components are
separated by *colons*, and the frame is specified as a single integer
at the end.

In practice, it is relatively easy to create a sprite using a number
of named layers in, e.g., GIMP, and export each layer based on its
name using a script.  That was done in the above example.
