# Build a nested packer

Build a grouped version of
[`monty_packer()`](https://mrc-ide.github.io/monty/reference/monty_packer.md)
with the same basic idea; convert between a vector representation of
some group of numbers to a named list of structured data, but with an
extra twist: this time the unstructured vector of numbers contains
values that correspond to multiple *groups* and some are shared across
groups while others vary between groups. This function does a lot of
bookkeeping in a relatively short amount of code, so you should be
familiar with the ideas in
[`monty_packer()`](https://mrc-ide.github.io/monty/reference/monty_packer.md)
before continuing.

## Usage

``` r
monty_packer_grouped(
  groups,
  scalar = NULL,
  array = NULL,
  fixed = NULL,
  process = NULL,
  shared = NULL
)
```

## Arguments

- groups:

  A character vector of group names. These must not be present within
  any of your `scalar` or `array` arguments.

- scalar:

  Names of scalars. This is similar for listing elements in `array` with
  values of 1, though elements in `scalar` will be placed ahead of those
  listed in `array` within the final parameter vector, and elements in
  `array` will have generated names that include square brackets.

- array:

  A list, where names correspond to the names of arrays and values
  correspond to their lengths. Multiple dimensions are allowed (so if
  you provide an element with two entries these represent dimensions of
  a matrix). Zero-length integer vectors or `NULL` values are counted as
  scalars, which allows you to put scalars at positions other than the
  front of the packing vector. In future, you may be able to use
  *strings* as values for the lengths, in which case these will be
  looked for within `fixed`.

- fixed:

  A named list of fixed data to be inserted into the final unpacked
  list; these will be added into the final list directly. In the
  parameter packer context, these typically represent additional pieces
  of data that your model needs to run, but which you are not performing
  inference on.

- process:

  An arbitrary R function that will be passed the final assembled list
  **for each group**; it may create any *additional* entries, which will
  be concatenated onto the original list. If you use this you should
  take care not to return any values with the same names as entries
  listed in `scalar`, `array` or `fixed`, as this is an error (this is
  so that `pack()` is not broken). We will likely play around with this
  process in future in order to get automatic differentiation to work.

- shared:

  Names of the elements in `scalar` and `array` that are shared among
  all groups.

## Value

An object of class `monty_packer_grouped`, which has the same elements
as `monty_packer`, though with slightly different effects.

- `names`: a function that returns a character vector of computed names;
  in the parameter packer context these are the names that your
  statistical model will use.

- `groups`: A function that returns your group names (the `groups`
  argument as supplied)

- `unpack`: A function for converting from an unstructured vector into a
  nested list. Each element of this list is conceptually the same as the
  result of `unpack()` from
  [`monty_packer()`](https://mrc-ide.github.io/monty/reference/monty_packer.md).

- `pack`: The inverse to `unpack()` but less commonly performed. Convert
  a nested list into an unstructured vector. Quite a lot of validation
  is required to make sure that the input has not been tampered with,
  and errors thrown while doing this validation may not be very
  interpretable.

- `index`: The nested version of the `index()` function in
  [`monty_packer()`](https://mrc-ide.github.io/monty/reference/monty_packer.md).
  The outer list is over groups, and the inner list contains the
  position within the original unstructured vector where this value can
  be found. It is not clear to us if this is a useful list.

- `subset`: A function that might eventually allow subsetting a grouped
  packer. Currently it just errors.

## Details

Recall from
[`monty_packer()`](https://mrc-ide.github.io/monty/reference/monty_packer.md)
that our original problem was to take an unstructured vector like

    | 1  2  3  4  5  6  7  |
    | a  b  c  d1 d2 d3 d4 |

and **unpack it** into a structured list like

    list(a = 1, b = 2, c = 3, d = 4:7)

Our aim here is to do the same but to allow some of these values (say
`b` and `c`) to be shared (constant) over groups while the others (`a`
and `d`) to vary by group. So for groups `x` and `y` we might try and
create something like

    list(
      list(x = list(a = 3, b = 1, c = 2, d = 4:7),
           y = list(a = 8, b = 1, c = 2, d = 9:12))

from a vector

    | 1  2  3  4  5  6  7  8  9  10 11 12 |
    | b  c  a  d1 d2 d3 d4 a  d1 d2 d3 d4 |
    | xy xy x  x  x  x  x  y  y  y  y  y  |

## Examples

``` r
p <- monty_packer_grouped(c("x", "y"), c("a", "b", "c", "d", "e"),
                          shared = c("b", "c"))
p$names()
#> [1] "b"    "c"    "a<x>" "d<x>" "e<x>" "a<y>" "d<y>" "e<y>"
p$unpack(1:8)
#> $x
#> $x$a
#> [1] 3
#> 
#> $x$b
#> [1] 1
#> 
#> $x$c
#> [1] 2
#> 
#> $x$d
#> [1] 4
#> 
#> $x$e
#> [1] 5
#> 
#> 
#> $y
#> $y$a
#> [1] 6
#> 
#> $y$b
#> [1] 1
#> 
#> $y$c
#> [1] 2
#> 
#> $y$d
#> [1] 7
#> 
#> $y$e
#> [1] 8
#> 
#> 
```
