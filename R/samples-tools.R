##' Thin results of running [monty_sample()], reducing autocorrelation
##' between samples and saving space. This function may be useful
##' before running onward simulations, or before saving output to
##' disk.
##'
##' # Limitations
##'
##' Subsetting parameters (`$pars`) and density (`$density`) is easy
##' enough, but the main use of this function is subsetting chains
##' that have observations, otherwise you could simply cast to
##' `samples_df` and use functions from the `posterior` package.
##'
##' We can only subset observations where the observer was able to
##' tidy them up into a nice array for us.  This will typically be the
##' case (for example when using odin/dust, trajectories will be in a
##' nice array).
##'
##' More specifically, an array is "nice" if the last two dimensions
##' represent samples and chains; in that case we subset along the
##' samples dimension and leave everything else alone.  For each
##' element of `$observations` that cannot be subsetted, we will issue
##' a warning.
##'
##' We cannot generally subset "details", and will pass that along
##' unmodified.
##'
##' @title Thin samples
##'
##' @param samples A `monty_samples` object, from running [monty_sample()]
##'
##' @param thinning_factor Optional integer thinning factor. If given,
##'   then we save every `thinning_factor`'th step.  So if
##'   `thinning_factor = 2` we save every second step, and if 10, we'd
##'   save every 10th.  We will always include the last point in the
##'   chain, and exclude points counting backwards.
##'
##' @param burnin Number of steps to discard as burnin from the start
##'   of the chain.
##'
##' @return A `monty_samples` object (as for [monty_sample()]),
##'   typically with fewer samples.
##'
##' @export
monty_samples_thin <- function(samples, thinning_factor = NULL, burnin = NULL) {
  assert_is(samples, "monty_samples")

  n_samples <- ncol(samples$pars)

  iteration <- seq_len(n_samples)
  keep <- rep(TRUE, n_samples)

  if (!is.null(burnin)) {
    assert_scalar_size(burnin, allow_zero = TRUE)
    if (burnin >= n_samples) {
      cli::cli_abort(
        "'burnin' must be smaller than {n_samples} for your samples")
    }
    if (burnin > 0) {
      keep <- keep & iteration > burnin
    }
  }

  if (!is.null(thinning_factor)) {
    assert_scalar_size(thinning_factor, allow_zero = TRUE)
    if (thinning_factor > 0) {
      keep <- keep & (n_samples - iteration) %% thinning_factor == 0
    }
  }

  if (all(keep)) {
    return(samples)
  }

  monty_samples_subset(samples, keep)
}


monty_samples_subset <- function(samples, i) {
  len <- dim(samples$density)

  samples$pars <- samples$pars[, i, , drop = FALSE]
  samples$density <- samples$density[i, , drop = FALSE]

  if (!is.null(samples$observations)) {
    skipped <- logical(length(samples$observations))
    for (j in seq_along(samples$observations)) {
      el <- samples$observations[[j]]
      d_el <- dim(el)
      can_subset <- !is.null(d_el) && length(d_el) >= 2 &&
        all(d_el[seq(to = length(d_el), length.out = 2)] == len)
      if (can_subset) {
        idx <- rep(list(rlang::missing_arg()), length(d_el))
        idx[[length(d_el) - 1]] <- i
        samples$observations[[j]] <-
          rlang::inject(el[!!!idx, drop = FALSE])
      } else {
        skipped[[j]] <- TRUE
      }
    }
    if (any(skipped)) {
      nm <- sprintf('observations[["%s"]]',
                    names(samples$observations)[skipped])
      cli::cli_warn("Cannot subset {squote(nm)}")
    }
  }

  samples
}
