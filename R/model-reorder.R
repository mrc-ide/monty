monty_model_reorder <- function(model, parameters) {
  if (!setequal(model$parameters, parameters)) {
    cli::cli_abort(
      paste("Can't reorder 'model' as 'parameters' contains different values",
            "to 'model$parameters'"))
  }
  monty_model(
    list(parameters = parameters,
         density = reorder_density(model, parameters),
         gradient = reorder_gradient(model, parameters),
         direct_sample = reorder_direct_sample(model, parameters),
         domain = reorder_domain(model, parameters)),
    properties = model$properties)
}


reorder_density <- function(model, parameters) {
  i <- match(model$parameters, parameters)
  function(x) {
    if (is.matrix(x)) {
      model$density(x[i, , drop = FALSE])
    } else {
      model$density(x[i])
    }
  }
}


reorder_gradient <- function(model, parameters) {
  if (!model$properties$has_gradient) {
    return(NULL)
  }
  i <- match(model$parameters, parameters)
  j <- match(parameters, model$parameters)
  function(x) {
    if (is.matrix(x)) {
      model$gradient(x[i, , drop = FALSE])[j, , drop = FALSE]
    } else {
      model$gradient(x[i])[j]
    }
  }
}


reorder_direct_sample <- function(model, parameters) {
  if (!model$properties$has_direct_sample) {
    return(NULL)
  }
  j <- match(parameters, model$parameters)
  function(rng) {
    model$direct_sample(rng)[j]
  }
}


reorder_domain <- function(model, parameters) {
  model$domain[match(parameters, rownames(model$domain)), , drop = FALSE]
}
