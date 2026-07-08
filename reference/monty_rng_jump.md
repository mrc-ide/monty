# Jump random number state

Jump random number state. There are two "lengths" of jumps; a normal
jump and a long jump. The normal jump is the distance between streams
within a random number state, so if you have a multi-stream rng this
shifts states left. The long jump is used to create distributed states.
We will properly explain all this once the interface stabilises.

## Usage

``` r
monty_rng_jump(state, n = 1)

monty_rng_long_jump(state, n = 1)
```

## Arguments

- state:

  Either a `monty_rng_state` object (created via
  [monty_rng_create](https://mrc-ide.github.io/monty/reference/monty_rng_create.md))
  or a raw vector suitable for creating one.

- n:

  The number of jumps to take (integer, 1 or more)

## Value

The `monty_rng_state` object (invisibly, modified in place) or a raw
vector, matching the input argument `state` (visibly).
