# Split text according to available text width

Dynamically wrap text.

## Usage

``` r
split_text_grob(
  text,
  x = grid::unit(0.5, "npc"),
  y = grid::unit(0.5, "npc"),
  width = grid::unit(1, "npc"),
  just = "centre",
  hjust = NULL,
  vjust = NULL,
  default.units = "npc",
  name = NULL,
  gp = grid::gpar(),
  vp = NULL
)
```

## Arguments

- text:

  (`string`)  
  the text to wrap.

- x:

  A numeric vector or unit object specifying x-values.

- y:

  A numeric vector or unit object specifying y-values.

- width:

  ([`grid::unit`](https://rdrr.io/r/grid/unit.html))  
  a unit object specifying maximum width of text.

- just:

  The justification of the text relative to its (x, y) location. If
  there are two values, the first value specifies horizontal
  justification and the second value specifies vertical justification.
  Possible string values are: `"left"`, `"right"`, `"centre"`,
  `"center"`, `"bottom"`, and `"top"`. For numeric values, 0 means left
  (bottom) alignment and 1 means right (top) alignment.

- hjust:

  A numeric vector specifying horizontal justification. If specified,
  overrides the `just` setting.

- vjust:

  A numeric vector specifying vertical justification. If specified,
  overrides the `just` setting.

- default.units:

  A string indicating the default units to use if `x` or `y` are only
  given as numeric vectors.

- name:

  A character identifier.

- gp:

  An object of class `"gpar"`, typically the output from a call to the
  function [`gpar`](https://rdrr.io/r/grid/gpar.html). This is basically
  a list of graphical parameter settings.

- vp:

  A Grid viewport object (or NULL).

## Value

A text `grob`.

## Details

This code is taken from `R Graphics by Paul Murell, 2nd edition`
