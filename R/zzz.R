.onLoad <- function(libname, pkgname) {
  reticulate::configure_environment(pkgname)
  rui::alert("{.pkg nmhsa} is still in its experimental lifecycle stage.")
  rui::alert("Use at your own risk, and submit issues here:")
  rui::alert("{.url https://github.com/rogiersbart/nmhsa/issues}")
  invisible()
}
