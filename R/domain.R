##' Check and expand a domain, where it is used alongside a
##' [mcstate_packer] object.  This can be used to expand domains for
##' logical parameters (e.g. a vector `b`) into its specific names
##' (e.g., `b[1]`, `b[2]`, etc) without having to rely on the
##' internals about how these names are constructed.
##'
##' @title Expand (and check) domain against a packer
##'
##' @param domain A two-column matrix as defined in [mcstate_model],
##'   with row names corresponding to either logical names (e.g., `b`)
##'   or specific names `b[1]` that are present in your packer.
##'   `NULL` is allowed where all parameters are defined over the
##'   entire real line.
##'
##' @param packer A [mcstate_packer] object
##'
##' @return A two dimensional matrix representing your domain, or
##'   `NULL` if `domain` was given as `NULL`.
##'
##' @export
##' @examples
##'
##' packer <- mcstate_packer(c("a", "b"), list(x = 3, y = c(2, 2)))
##' mcstate_domain_expand(NULL, packer)
##' mcstate_domain_expand(rbind(x = c(0, 1)), packer)
##' mcstate_domain_expand(rbind(x = c(0, 1), "x[2]" = c(0, Inf)), packer)
##' mcstate_domain_expand(rbind(x = c(0, 1), "y" = c(0, Inf)), packer)
mcstate_domain_expand <- function(domain, packer) {
  assert_is(packer, "mcstate_packer")
  if (is.null(domain)) {
    return(domain)
  }

  if (!is.matrix(domain)) {
    cli::cli_abort("Expected 'domain' to be a matrix",
                   arg = "domain")
  }
  if (ncol(domain) != 2) {
    cli::cli_abort(
      "Expected 'domain' to have 2 columns but it had {ncol(domain)}",
      arg = "domain")
  }
  if (nrow(domain) == 0) {
    return(NULL)
  }
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

  nms_full <- packer$parameters
  nms_map <- packer$unpack(nms_full)
  nms_logical <- names(nms_map)

  i <- nms %in% nms_logical & !(nms %in% intersect(nms_logical, nms_full))
  err <- !(i | nms %in% nms_full)
  if (any(err)) {
    cli::cli_abort(
      "Unknown {?entry/entries} in 'domain' rownames: {squote(nms[err])}",
      arg = "domain")
  }

  if (any(i)) {
    nms_expand <- nms[i]
    extra <- unname(domain)[
      rep(which(i), lengths(nms_map[nms_expand])), , drop = FALSE]
    rownames(extra) <- unlist(nms_map[nms_expand], FALSE, FALSE)
    j <- !(rownames(extra) %in% rownames(domain))
    domain <- rbind(extra[j, , drop = FALSE],
                    domain[!i, , drop = FALSE])
  }

  domain[order(match(rownames(domain), packer$parameters)), , drop = FALSE]
}
