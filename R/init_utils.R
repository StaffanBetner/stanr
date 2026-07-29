# Normalize the public initialization forms to the named list consumed by the
# native var_context adapter. A scalar is the CmdStan initialization radius.
normalize_init <- function(init) {
  if (isTRUE(length(init) == 1L && is.null(names(init)) &&
            is.numeric(init) && !is.na(init) && init >= 0)) {
    return(list())
  }
  if (is.list(init)) {
    return(init)
  }
  if (is.numeric(init)) {
    if (is.null(names(init)) || any(!nzchar(names(init)))) {
      stop("Numeric init must be a named vector of constrained parameters.",
           call. = FALSE)
    }
    return(as.list(init))
  }
  stop("init must be a non-negative radius, a named list, or a named numeric vector.",
       call. = FALSE)
}

init_radius <- function(init) {
  if (isTRUE(length(init) == 1L && is.null(names(init)) &&
            is.numeric(init) && !is.na(init) && init >= 0)) {
    return(as.double(init))
  }
  2
}

# Create the data context, concrete model, and the model-local autodiff bridge
# together so the bridge's raw model pointer remains valid for the native call.
new_model_instance <- function(stanmod, data, seed) {
  data_ptr <- .Call(`r_data_context`, data)
  model_ptr <- stanmod$new_model(data_ptr, seed)
  bridge_ptr <- stanmod$new_model_bridge(model_ptr)
  list(data = data_ptr, model = model_ptr, bridge = bridge_ptr)
}
