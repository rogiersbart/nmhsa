#' Access to the Python wrapper examples
#'
#' @param algorithm One of "hsa", "mhsa", "nmhsa" or "3d".
#'
#' @return A 2D or 3D array
#' @export
wrapper <- function(algorithm) {
  prepare_python()
  rui::begin("Reconstructing")
  rec <- reticulate::py$wrapper(ti = cement, mode = toupper(gsub("sa", "-SA", algorithm)))
  rui::succeed()
  invisible(as.nmhsa_array(rec))
}
