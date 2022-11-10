prepare_python <- function() {
  if (!is.null(reticulate::py)) return(invisible())
  rui::begin("Preparing the Python backend")
  locals <- reticulate::py_run_string("locals()") |> names()
  if (! "NMH_SA" %in% locals) {
    reticulate::source_python(
      system.file("python", "nmhsa.py", package = "nmhsa")
    )
  }
  if (! "rui" %in% locals) rui:::python()
  py <- reticulate::py
  rui::succeed()
}

is.wholenumber <- function(x, tol = .Machine$double.eps^0.5) {
  abs(x - round(x)) < tol
}

check_integer <- function(x, argument = NULL) {
  if (! all(is.wholenumber(x))) {
    if (! is.null(argument)) {
      rui::alert("Integer input required for {.arg {argument}}")
    } else {
      rui::alert("Integer input required")
    }
    rui::stop("Issue with function arguments")
  }
  if ("array" %in% class(x)) {
    x[] <- c(x) |> check_integer()
    return(x)
  }
  as.integer(x)
}

check_length <- function(x, length, argument = NULL) {
  if (length(x) != length) {
    if (length(x) == 1) return(rep(x, length))
    if (! is.null(argument)) {
      rui::alert("Argument {.arg {argument}} is not of the required length")
    } else {
      rui::alert("Argument is not of the required length")
    }
    rui::stop("Issue with function arguments")
  }
  x
}
