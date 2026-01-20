# Build a packer

Build a packer, which can be used to translate between an unstructured
vector of numbers (the vector being updated by an MCMC for example) and
a structured list of named values, which is easier to program against.
This is useful for the bridge between model parameters and a model's
implementation, but it is also useful for the state vector in a
state-space model. We refer to the process of taking a named list of
scalars, vectors and arrays and converting into a single vector
"packing" and the inverse "unpacking".

## Usage

``` r
monty_packer(scalar = NULL, array = NULL, fixed = NULL, process = NULL)
```

## Arguments

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

  An arbitrary R function that will be passed the final assembled list;
  it may create any *additional* entries, which will be concatenated
  onto the original list. If you use this you should take care not to
  return any values with the same names as entries listed in `scalar`,
  `array` or `fixed`, as this is an error (this is so that `pack()` is
  not broken). We will likely play around with this process in future in
  order to get automatic differentiation to work.

## Value

An object of class `monty_packer`, which has elements:

- `names`: a function that returns a character vector of computed names;
  in the parameter packer context these are the names that your
  statistical model will use.

- `unpack`: a function that can unpack an unstructured vector (say, from
  your statistical model parameters) into a structured list (say, for
  your generative model)

- `pack`: a function that can pack your structured list of data back
  into a numeric vector, for example suitable for a statistical model.
  This ignores values created by a `preprocess` function and present in
  `fixed`.

- `index`: a function which produces a named list where each element has
  the name of a value in `scalar` or `array` and each value has the
  indices within an unstructured vector where these values can be found,
  in the shape of the data that would be unpacked. This is of limited
  most use to most people.

- `subset`: an experimental interface which can be used to subset a
  packer to a packer for a subset of contents. Documentation will be
  provided once the interface settles, but this is for advanced use
  only!

- `inputs`: inputs that could be used to reconstitute this packer.

## Details

There are several places where it is most convenient to work in an
unstructured vector:

- An MCMC is typically discussed as a the updating of some vector `x` to
  another `x'`

- An optimisation algorithm will try and find a set of values for a
  vector `x` that minimises (or maximises) some function `f(x)`

- An ode solver works with a vector `x(t)` (`x` at time `t`) and
  considers `x(t + h)` by computing the vector of derivatives `dx(t)/dt`

In all these cases, the algorithm that needs the vector of numbers knows
nothing about what they represent. Commonly, these will be a packed
vector of parameters. So our vector `x` might actually represent the
parameters `a`, `b` and `c` in a vector as `[a, b, c]` - this is a very
common pattern, and you have probably implemented this yourself.

In more complex settings, we might want our vector `x` to collect more
structured quantities. Suppose that you are fitting a model with an
age-structured or sex-structured parameter. Rather than having a series
of scalars packed into your vector `x` you might have a series of values
destined to be treated as a vector:

    | 1  2  3  4  5  6  7  |
    | a  b  c  d1 d2 d3 d4 |

So here we might have a vector of length 7, where the first three
elements will represent be the scalar values `a`, `b` and `c` but the
next four will be a vector `d`.

Unpacked, this might be written as:

    list(a = 1, b = 2, c = 3, d = 4:7)

The machinery here is designed to make these transformations simple and
standardised within monty, and should be flexible enough for many
situations. We will also use these from within `dust2` and `odin2` for
transformations in and out of vectors of ODE state.

## When to use `process`

The `process` function is a get-out-of-jail function designed to let you
do arbitrary transformations when unpacking a vector. In general, this
should not be the first choice to use because it is less easy to reason
about by other tooling (for example, as we develop automatic
differentiation support for use with the HMC algorithm, a `process`
function will be problematic because we will need to make sure we can
differentiate this process). However, there are cases where it will be
only way to achieve some results.

Imagine that you are packing a 2x2 covariance matrix into your vector in
order to use within an MCMC or optimisation algorithm. Ultimately, our
unpacked vector will need to hold four elements (`b11`, `b12`, `b21`,
`b22`), but there are only three distinct values as the two off-diagonal
elements will be the same (i.e., `b12 == b21`). So we might write this
passing in `b_raw = 3` to `array`, so that our unpacked list holds
`b_raw = c(b11, b12, b22)`. We would then write `process` as something
like:

    process <- function(x) {
      list(b = matrix(x$b_raw[c(1, 2, 2, 3)], 2, 2))
    }

which creates the symmetric 2x2 matrix `b` from `b_raw`.

## Unpacking matrices

If you do not use `fixed` or `process` when defining your packer, then
you can use `$unpack()` with a matrix or higher-dimensional output.
There are two ways that you might like to unpack this sort of output.
Assume you have a matrix `m` with 3 rows and 2 columns; this means that
we have two sets of parameters or state (one per column) and 3 states
within each; this is the format that MCMC parameters will be in for
example.

The first would to be return a list where the `i`th element is the
result of unpacking the `i`th parameter/state vector. You can do this by
running

    apply(m, 2, p$unpack)

The second would be to return a named list with three elements where the
`ith` element is the unpacked version of the `i`th state. In this case
you can pass the matrix directly in to the unpacker:

    p$unpack(m)

When you do this, the elements of `m` will acquire an additional
dimension; scalars become vectors (one per set), vectors become matrices
(one column per set) and so on.

This approach generalises to higher dimensional input, though we suspect
you'll spend a bit of time head-scratching if you use it.

## Packing lists into vectors and matrices

The unpacking operation is very common - an MCMC proceeds, produces an
unstructured vector, and you unpack it into a list in order to be able
to easily work with it. The reverse is much less common, where we take a
list and convert it into a vector (or matrix, or multidimensional
array). Use of this direction ("packing") may be more common where using
packers to work with the output of state-space models (e.g. in
[odin2](https://mrc-ide.github.io/odin2) or
[dust2](https://mrc-ide.github.io/dust2), which use this machinery).

The input to `pack()` will be the shape that `unpack()` returned; a
named list of numerical vectors, matrices and arrays. The names must
correspond to the names in your packer (i.e., `scalar` and the names of
`array`). Each element has dimensions

    <...object, ...residual>

where `...object` is the dimensions of the data itself and `...residual`
is the dimensions of the hypothetical input to `pack`.

There is an unfortunate ambiguity in R's lack of true scalar types that
we cannot avoid. It is hard to tell the difference packing a vector vs
packing an array where all dimensions are 1. See the examples, and
please let us know if the behaviour needs changing.

## Examples

``` r
# Here's a really simple example
p <- monty_packer(c("a", "b", "c"))
p
#> 
#> ── <monty_packer> ──────────────────────────────────────────────────────────────
#> ℹ Packing 3 parameters: 'a', 'b', and 'c'
#> ℹ Use '$pack()' to convert from a list to a vector
#> ℹ Use '$unpack()' to convert from a vector to a list
#> ℹ See `?monty_packer()` for more information

p$pack(list(a = 1, b = 2, c = 3))
#> [1] 1 2 3
p$unpack(1:3)
#> $a
#> [1] 1
#> 
#> $b
#> [1] 2
#> 
#> $c
#> [1] 3
#> 

# Sometimes we have a vector embedded in our parameters:
p <- monty_packer(c("a", "b"), list(v = 4))
p$pack(list(a = 1, b = 2, v = c(6, 7, 8, 9)))
#> [1] 1 2 6 7 8 9
p$unpack(c(1, 2, 6, 7, 8, 9))
#> $a
#> [1] 1
#> 
#> $b
#> [1] 2
#> 
#> $v
#> [1] 6 7 8 9
#> 

# Or a higher dimensional structure such as a matrix:
p <- monty_packer(c("a", "b"), list(m = c(2, 2)))
p$unpack(c(1, 2, 6, 7, 8, 9))
#> $a
#> [1] 1
#> 
#> $b
#> [1] 2
#> 
#> $m
#>      [,1] [,2]
#> [1,]    6    8
#> [2,]    7    9
#> 

# You can use a packer to set "fixed" parameters that do not vary
# with the underlying model being fit, but are required by your model.
# This is simpler than the "closure" approach used previously in our
# mcstate package and also easier to accommodate with differentiable
# models:
p <- monty_packer(
  c("a", "b"),
  fixed = list(d = data.frame(n = 1:3, m = runif(3))))
p$unpack(1:2)
#> $a
#> [1] 1
#> 
#> $b
#> [1] 2
#> 
#> $d
#>   n          m
#> 1 1 0.44670247
#> 2 2 0.37151118
#> 3 3 0.02806097
#> 
p$pack(p$unpack(1:2))
#> [1] 1 2

# The example from above, where we create a symmetric 2 x 2 matrix
# from a 3-element vector, alongside a scalar:
p <- monty_packer(
  scalar = "a",
  array = list(b_flat = 3),
  process = function(p) list(b = matrix(p$b_flat[c(1, 2, 2, 3)], 2, 2)))

# Unpacking we see "b_flat" is still in the list, but "b" is our
# symmetric matrix:
p$unpack(1:4)
#> $a
#> [1] 1
#> 
#> $b_flat
#> [1] 2 3 4
#> 
#> $b
#>      [,1] [,2]
#> [1,]    2    3
#> [2,]    3    4
#> 

# The processed elements are ignored on the return pack:
p$pack(list(a = 1, b_flat = 2:4, b = matrix(c(2, 3, 3, 4), 2, 2)))
#> [1] 1 2 3 4
p$pack(list(a = 1, b_flat = 2:4))
#> [1] 1 2 3 4

# R lacks scalars, which means that some packers will unpack
# different inputs to the same outputs:
p <- monty_packer(c("a", "b"))
p$unpack(1:2)
#> $a
#> [1] 1
#> 
#> $b
#> [1] 2
#> 
p$unpack(cbind(1:2))
#> $a
#> [1] 1
#> 
#> $b
#> [1] 2
#> 

# This means that we can't reliably pack these inputs in a way
# that guarantees round-tripping is possible.  We have chosen to
# prioritise the case where a *single vector* is round-trippable:
p$pack(list(a = 1, b = 2))
#> [1] 1 2

# This ambiguity goes away if unpacking matices with more than one
# column:
p$unpack(matrix(1:6, 2, 3))
#> $a
#> [1] 1 3 5
#> 
#> $b
#> [1] 2 4 6
#> 
```
