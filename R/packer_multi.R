## TODO:
## * exclude for dropping *some* combinations (e.g., no something in wales)
## * fixed; nested list for non-shared things, or perhaps a data.frame approach?
## * process; this is actually quite tricky
monty_packer_grouped <- function(groups, scalar = NULL, array = NULL,
                                 shared = NULL) {
  assert_character(groups)
  n_groups <- length(groups)
  if (n_groups < 2) {
    cli::cli_abort("Expected at least two groups", arg = "groups")
  }

  ## We might want to just refactor the checks from here rather than
  ## directly using it.
  p <- monty_packer(scalar, array)
  nms <- names(p$index())

  dups <- duplicate_values(shared)
  if (length(dups) > 0) {
    cli::cli_abort(
      c("Elements of 'shared' must be unique",
        i = "Found {length(dups)} duplicate{?s}: {collapseq(dups)}"),
      arg = "shared")
  }

  err <- setdiff(names(shared), nms)
  if (length(err) > 0) {
    cli::cli_abort("Unknown parameter{?s} in 'shared': {squote(err)}",
                   arg = "shared")
  }
  varied <- setdiff(scalar, shared)

  len <- length(scalar)

  if (!is.null(array)) {
    cli::cli_abort("Arrays not yet handled")
  }

  ## idx <- list(shared = list(),
  ##             varied = list(),
  ##             scalar = scalar,
  ##             array = names(array))
  ## shape <- list()

  ## idx[scalar] <- set_names(as.list(seq_along(scalar)), scalar)
  
  ## idx <- list(shared = match(shared, scalar),
  ##             varied = 
              
  unpack <- function(x) {
    if (!is.null(dim(x))) {
      cli::cli_abort("Can't unpack grouped arrays yet")
    }
    browser()
    base <- set_names(vector("list", len), scalar)
    base[intersect(scalar, shared)] <- x[match(shared, scalar)]
    ret <- rep(list(base), n_groups)
    xv <- matrix(x[-seq_along(shared)], ncol = n_groups)
    i <- match(varied, scalar)
    j <- intersect(scalar, varied)
    for (k in seq_len(n_groups)) {
      ret[[k]][j] <- xv[i, k]
    }
    set_names(ret, groups)
  }

  pack <- function(p) {
    browser()
  }

  index <- function() {
    browser()
  }

  

  ## Quick and dirty parameter building for now, will need changing
  ## once we support arrays
  stopifnot(length(array) == 0)
  nms_varied <- setdiff(scalar, shared)
  parameters <- c(shared,
                  sprintf("%s{%s}",
                          rep(nms_varied, n_groups),
                          rep(groups, each = length(nms_varied))))

  

  ret <- list(parameters = parameters,
              unpack = unpack,
              pack = pack,
              index = function() idx)
  # class(ret) <- "monty_packer"
  ret
}
