#' Merge neighbouring pixels
#'
#' This functionality was originally introduced to remove inclusions of certain
#' phases within another phase. We are however simply looking for neighbouring
#' pixels here. As this is still effective with small inclusions, for larger
#' ones, repeated calls of this function may be required. Note however that this
#' also affects pixels of the phase to modify that are not inclusions but just
#' neighbouring pixels of the target phase.
#'
#' @param img A 2D array.
#' @param phase The phase to modify.
#' @param into The phase to change pixels to.
#'
#' @returns The modified 2D array.
#' @seealso [phase_split()], [nmhsa()]
#' @export
phase_merge <- function(
  img,
  phase,
  into
) {
  # TODO currently this is changing touching pixels, not actually inclusions
  rui::alert("Only merging touching pixels of phase {phase} into {into}")
  prepare_python()
  reticulate::py$hole_removement(img, phase, into, phase) |>
    as.nmhsa_array()
}

#' Split into subphases
#'
#' This function splits a certain phase into two subphases based on a size
#' threshold, which makes sense in the reconstruction framework if the two
#' subphases behave different structurally.
#'
#' @param img A 2D array.
#' @param phase The phase to modify.
#' @param larger_than The threshold for splitting (in pixels).
#' @param into The phase to change pixels to.
#'
#' @returns The modified 2D array.
#' @seealso [phase_merge()], [nmhsa()]
#' @export
phase_split <- function(
  img,
  phase,
  larger_than,
  into = max(c(img)) + 1
) {
  prepare_python()
  reticulate::py$phasesplit(img, phase, larger_than, c(into, phase)) |>
    as.nmhsa_array()
}

phases <- function(x) unique(c(x))
nphases <- function(x) length(phases(x))
