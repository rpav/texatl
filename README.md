# texatl

This is a simple texture atlas generator.  Currently, it only
generates an atlas and font metrics for a font using
[cl-freetype2](https://github.com/rpav/cl-freetype2) and
[cl-cairo2](https://github.com/rpav/cl-cairo2).  Shortly, it will
import an arbitrary set of PNG images (and possibly other formats) and
generate an atlas from those, as well.

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

## Fonts

### make-font-atlas

The primary function for generating an atlas is `MAKE-FONT-ATLAS`:

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

## do-texatl-string

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
