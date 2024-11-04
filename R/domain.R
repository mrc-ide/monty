##' Check and expand a domain, where it is used alongside a
##' [monty_packer] object.  This can be used to expand domains for
##' logical parameters (e.g. a vector `b`) into its specific names
##' (e.g., `b[1]`, `b[2]`, etc) without having to rely on the
##' internals about how these names are constructed.
##'
##' @title Expand (and check) domain against a packer
##'
##' @param domain A two-column matrix as defined in [monty_model],
##'   with row names corresponding to either logical names (e.g., `b`)
##'   or specific names `b[1]` that are present in your packer.
##'   `NULL` is allowed where all parameters are defined over the
##'   entire real line.
##'
##' @param packer A [monty_packer] object
##'
##' @return A two dimensional matrix representing your domain, or
##'   `NULL` if `domain` was given as `NULL`.
##'
##' @export
##' @examples
##'
##' packer <- monty_packer(c("a", "b"), list(x = 3, y = c(2, 2)))
##' monty_domain_expand(NULL, packer)
##' monty_domain_expand(rbind(x = c(0, 1)), packer)
##' monty_domain_expand(rbind(x = c(0, 1), "x[2]" = c(0, Inf)), packer)
##' monty_domain_expand(rbind(x = c(0, 1), "y" = c(0, Inf)), packer)
monty_domain_expand <- function(domain, packer) {
  assert_is(packer, c("monty_packer", "monty_packer_grouped"))
  if (is.null(domain)) {
    return(domain)
  }

  if (!is.matrix(domain)) {
    cli::cli_abort("Expected 'domain' to be a matrix",
                   arg = "domain")
  }
  if (ncol(domain) != 2) {
    cli::cli_abort(
      "Expected 'domain' to have 2 columns, but it had {ncol(domain)}",
      arg = "domain")
  }
  if (nrow(domain) == 0) {
    return(NULL)
  }

  ## Below here needs some actual work for grouped packers
  nms <- rownames(domain)
  if (is.null(nms)) {
    cli::cli_abort("Expected 'domain' to have row names", arg = "domain")
  }
  if (anyDuplicated(nms)) {
    dups <- unique(nms[duplicated(nms)])
    cli::cli_abort(
      "Duplicated {?entry/entries} in 'domain' rownames: {squote(dups)}",
      arg = "domain")
  }

  nms_full <- packer$names()
  nms_map <- packer$unpack(nms_full)

  is_grouped <- inherits(packer, "monty_packer_grouped")

  if (is_grouped) {
    nms_logical <- unique(unlist(lapply(nms_map, names), FALSE, FALSE))
  } else {
    nms_logical <- names(nms_map)
  }

  i <- nms %in% nms_logical & !(nms %in% intersect(nms_logical, nms_full))
  err <- !(i | nms %in% nms_full)
  if (any(err)) {
    cli::cli_abort(
      "Unknown {?entry/entries} in 'domain' rownames: {squote(nms[err])}",
      arg = "domain")
  }

  if (any(i)) {
    nms_expand <- nms[i]
    if (is_grouped) {
      j <- unlist(lapply(unname(nms_map), function(el) {
        nms_el <- intersect(nms, names(el))
        set_names(rep(match(nms_el, nms), lengths(el[nms_el])),
                  unlist(el[nms_el]))
      }))
    } else {
      j <- set_names(rep(which(i), lengths(nms_map[nms_expand])),
                     unlist(nms_map[nms_expand], FALSE, FALSE))
    }
    extra <- unname(domain)[j, , drop = FALSE]
    rownames(extra) <- names(j)
    keep <- !(rownames(extra) %in% rownames(domain))
    domain <- rbind(extra[keep, , drop = FALSE],
                    domain[!i, , drop = FALSE])
  }

  domain[order(match(rownames(domain), packer$names())), , drop = FALSE]
}
