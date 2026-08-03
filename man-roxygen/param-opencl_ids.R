#' @param opencl_ids (integer vector) `c(platform_id, device_id)` identifying
#'   the OpenCL platform/device to run on. Only meaningful for a model
#'   compiled with `use_opencl = TRUE` (see [stan_model()]); errors if the
#'   model was not compiled with OpenCL support. Defaults to `NULL`, meaning
#'   `select_opencl_device()` is never called and the platform/device baked
#'   in at compile time (0/0) is used.
