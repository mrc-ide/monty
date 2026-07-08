# Trace random number calls

Trace calls to R's random-number-generating functions, to detect
unexpected use of random number generation outside of monty's control.

## Usage

``` r
with_trace_random(code, max_calls = 5, show_stack = FALSE)
```

## Arguments

- code:

  Code to run with tracing on

- max_calls:

  Maximum number of calls to report. The default is 5

- show_stack:

  Logical, indicating if we should show the stack at the point of the
  call

## Value

The result of evaluating `code`
