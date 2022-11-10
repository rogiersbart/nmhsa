# algorithms ----

sa <- function() {
  # NOTE for just binary image? or multiphase but all together, not hierarchical?
}

#' Hierarchical simulated annealing
#'
#' This is the simplest algorithm available at the moment, which is capable of
#' multiphase reconstruction using the hierarchical approach (phases are added
#' one by one).
#'
#' @param ti The original training image (2D array).
#' @param dimensions Dimensions for the reconstruction (vector of length 2 or
#'   3). Defaults to the dimensions of `ti`.
#' @param start_after Number of iterations for determining the initial simulated
#'   annealing temperature. Defaults to 0.1% of the total amount of pixels in
#'   the reconstruction.
#' @param stop_after Maximum number of iterations. Defaults to the amount of
#'   pixels in the reconstruction. Can be a vector for different values per
#'   phase.
#' @param stop_rejected Maximum number of consecutively rejected iterations.
#'   Defaults to 1% of the total amount of pixels in the reconstruction.
#' @param stop_at Target precision. Defaults to `1e-6`.
#' @param order Integer vector with the phase order for hierarchical simulation.
#'   Defaults to the least to the most occurring phase.
#' @param distance Distance, in pixels, up to which to investigate the
#'   structural descriptors. Defaults to 1/4th of the minimum of `dim(ti)` and
#'   `dimensions`.
#' @param cool Simulated annealing cooling factor. Must be lower than one.
#'   Defaults to `0.9`.
#' @return A reconstructed 2D or 3D array.
#' @references [https://doi.org/10.1103/PhysRevE.100.053316]
#' @export
#' @seealso [mhsa()], [nmhsa()]
hsa <- function(
    ti,
    dimensions = NULL,
    start_after = NULL,
    stop_after = NULL,
    stop_rejected = NULL,
    stop_at = 1e-6,
    order = NULL,
    distance = NULL,
    cool = 0.9
) {
  prepare_python()
  rui::begin("Checking inputs")
  on.exit(rui::clear())
  hsa <- prep_args_hsa(ti, dimensions, start_after, stop_after, stop_rejected, stop_at, order, distance, cool)
  rui::proceed("Reconstructing")
  algorithm <- reticulate::py$NMH_SA
  if (length(hsa$sgd) == 3) algorithm <- reticulate::py$NMH_SA2Dto3D
  reticulate::py_capture_output(
    rec <- algorithm(
      tiini = hsa$tiini,
      order = hsa$order,
      length = hsa$length,
      nps = hsa$nps,
      nt = hsa$nt,
      nstop = hsa$nstop,
      acc = hsa$acc,
      sgd= hsa$sgd,
      lam = hsa$lam,
      # TODO check is 6 is indeed best available option in 3D?
      nrdir = if (length(hsa$sgd) == 2) 4L else 9L,
      gridlevel = rep(1L, length(hsa$order))
    )
  )
  rui::succeed()
  rec <- rec - hsa$correction
  rec |> as.nmhsa_array()
}
prep_args_hsa <- function(
  ti,
  dimensions,
  start_after,
  stop_after,
  stop_rejected,
  stop_at,
  order,
  distance,
  cool
) {

  # TODO check dimensions are larger then distances

  # tiini
  tiini <- ti |> check_integer("ti")
  nsteps <- length(unique(c(tiini)))

  # order
  if (is.null(order)) {
    order <- table(tiini) |>
      sort() |>
      names() |>
      as.integer()
  } else {
    order <- order |> check_integer("order")
  }
  if (length(order) > nsteps) {
    order <- order[1:nsteps]
    rui::warn(
      capture.output(
        rui::tell("Argument {.arg order} should be of length {nsteps}.")
      )[1]
    )
    # TODO add this to rui::warn and rui::stop!!!
    rui::warn("Using {.code order[1:{nsteps}]} instead.")
  }

  # sgd
  sgd <- if (is.null(dimensions)) dim(tiini) else dimensions |> check_integer("dimensions")

  # length
  if (is.null(distance)) {
    length <- rep(min(dim(tiini), dim(sgd)) %/% 4, length(order)) |>
      as.integer()
  } else {
    length <- distance |> check_integer("distance")
  }
  # TODO check length of length and modify with warning potentially
  # if length one, recycle, if too large, subset, both with warning?
  if (length(length) == 1) {
    length <- rep(length, nsteps)
  }

  # acc
  acc <- stop_at |> check_length(1, "stop_at")


  # nt
  if (is.null(stop_after)) stop_after <- prod(sgd)
  nt <- stop_after |> check_integer("stop_after")
  if (length(nt) == 1) {
    nt <- rep(nt, nsteps)
  }

  # nps
  if (is.null(start_after)) start_after <- as.integer(max(nt) * 0.001)
  nps <- start_after |> check_integer("start_after")

  # nstop
  if (is.null(stop_rejected)) stop_rejected <- as.integer(max(nt) * 0.01)
  nstop <- stop_rejected |> check_integer("stop_rejected")

  # lam
  lam <- cool

  # correct all phase numbers so first one is 1
  correction <- 1 - min(tiini)
  tiini <- tiini + correction
  list(tiini = tiini,
       nsteps = nsteps,
       order = order,
       length = length,
       nt = nt,
       nps = nps,
       nstop = nstop,
       acc = acc,
       sgd = sgd,
       lam = lam,
       correction = correction)
}

#' Multiresolution hierarchical simulated annealing
#'
#' This algorithm extends [hsa()] by introducing different grid levels, or
#' resolutions, which should result in improved and more efficient
#' reconstruction of larger particles.
#'
#' @inheritParams hsa
#' @param levels Amount of grid levels (or resolutions) to use for the
#'   reconstruction. Defaults to `2`. Can be a vector to use different levels
#'   for different phases.
#' @return A reconstructed 2D or 3D array.
#' @references [https://doi.org/10.1103/PhysRevE.100.053316]
#' @export
#' @seealso [hsa()], [nmhsa()]
mhsa <- function(
    ti,
    dimensions = NULL,
    start_after = NULL,
    stop_after = NULL,
    stop_rejected = NULL,
    stop_at = 1e-6,
    order = NULL,
    distance = NULL,
    cool = 0.9,
    levels = 2
) {
  prepare_python()
  rui::begin("Checking inputs")
  on.exit(rui::clear())
  hsa <- prep_args_hsa(ti, dimensions, start_after, stop_after, stop_rejected, stop_at, order, distance, cool)
  mhsa <- prep_args_mhsa(hsa$nsteps, levels)
  rui::proceed("Reconstructing")
  algorithm <- reticulate::py$NMH_SA
  if (length(hsa$sgd) == 3) algorithm <- reticulate::py$NMH_SA2Dto3D
  reticulate::py_capture_output(
    rec <- algorithm(
      tiini = hsa$tiini,
      order = hsa$order,
      length = hsa$length,
      nps = hsa$nps,
      nt = hsa$nt %/% mhsa$gridlevel,
      nstop = hsa$nstop,
      acc = hsa$acc,
      sgd= hsa$sgd,
      lam = hsa$lam,
      # TODO check is 6 is indeed best available option in 3D?
      nrdir = if (length(hsa$sgd) == 2) 4L else 9L,
      gridlevel = mhsa$gridlevel
    )
  )
  rui::succeed()
  rec <- rec - hsa$correction
  rec |> as.nmhsa_array()
}
prep_args_mhsa <- function(nsteps, levels) {
  gridlevel = levels |> check_integer("levels")
  if (length(levels) == 1) levels <- rep(levels, nsteps)
  list(gridlevel = gridlevel)
}

#' Nested multiresolution hierarchical simulated annealing
#'
#' This algorithm extends [mhsa()] by introducing the nested approach, meaning
#' it automatically handles identified subphases (which are split by the user,
#' and merged again by the algorithm), and allows handling things like
#' inclusions by a splitting step at the end of the simulation (phases merged,
#' or rather rearranged, by the user, and split again by the algorithm).
#'
#' A more general approach to the phase merging and splitting would be possible,
#' where the user has control of every step in the simulation, but this is
#' currently not implemented.
#'
#' @inheritParams hsa
#' @inheritParams mhsa
#' @param ti_merged Merged version of `ti`. Typically obtained through
#'   [phase_merge()].
#' @param ti_splitted Splitted version of `ti_merged`. Typically obtained
#'   through [phase_split()].
#' @param merge_pairs List of length 2 vectors, indicating for which pairs of
#'   phases optimisation should again be performed after merging all subphases
#'   again to their original phase (transition from the state of `ti_splitted`
#'   to the state of `ti_merged`).
#' @param merge_distance Distance, in pixels, up to which to investigate the
#'   structural descriptors for the merging step. Defaults to 1/4th of the
#'   minimum of `dim(ti)` and `dimensions`. Should be a vector of the same
#'   length as `merge_pairs`.
#' @param merge_stop_after Maximum number of iterations for the merging step.
#'   Defaults to the amount of pixels in the reconstruction. Can be a vector for
#'   different values per phase.
#' @param split_distance Distance, in pixels, up to which to investigate the
#'   structural descriptors for the splitting step. Defaults to 1/4th of the
#'   minimum of `dim(ti)` and `dimensions`.
#' @param split_stop_after Maximum number of iterations for the splitting step.
#'   Defaults to the amount of pixels in the reconstruction.
#' @return A reconstructed 2D or 3D array.
#' @references [https://doi.org/10.1103/PhysRevE.100.053316]
#' @export
#' @seealso [hsa()], [mhsa()], [phase_merge()], [phase_split()]
nmhsa <- function(
    ti,
    dimensions = NULL,
    start_after = NULL,
    stop_after = NULL,
    stop_rejected = NULL,
    stop_at = 1e-6,
    order = NULL,
    distance = NULL,
    cool = 0.9,
    levels = 2,
    ti_merged = NULL,
    ti_splitted = NULL,
    merge_pairs = NULL,
    merge_distance = NULL,
    merge_stop_after = NULL,
    split_distance = NULL,
    split_stop_after = NULL
) {
  # TODO python ui is a mess; try to simplify things:
  # - maybe use ti (orig img) and ti_prep (including merges & splits), and
  #   derive tiini, mergephasein, mergephaseto, ti_2, split_target,
  #   split_targeto, targetm from it?
  # - everything that has to be specified order-1 times, recycle single value
  #   if given
  # TODO try to come up with good defaults
  # - order from least to most occurring phase?
  # -
  prepare_python()
  rui::begin("Checking inputs")
  on.exit(rui::clear())
  hsa <- prep_args_hsa(
    ti_splitted,
    dimensions,
    start_after,
    stop_after,
    stop_rejected,
    stop_at,
    order,
    distance,
    cool
  )
  mhsa <- prep_args_mhsa(hsa$nsteps, levels)
  nmhsa <- prep_args_nmhsa(
    hsa$correction,
    hsa$sgd,
    ti,
    ti_merged,
    ti_splitted,
    split_distance,
    split_stop_after,
    merge_pairs,
    merge_distance,
    merge_stop_after
  )
  rui::proceed("Reconstructing")
  algorithm <- reticulate::py$NMH_SA
  if (length(hsa$sgd) == 3) algorithm <- reticulate::py$NMH_SA2Dto3D
  reticulate::py_capture_output(
    rec <- algorithm(
      tiini = nmhsa$tiini,
      order = hsa$order |> as.list(),
      length = hsa$length |> as.list(),
      nps = hsa$nps,
      nt = hsa$nt %/% mhsa$gridlevel |> as.list(),
      nstop = hsa$nstop,
      acc = hsa$acc,
      sgd= hsa$sgd,
      lam = hsa$lam,
      # TODO check is 6 is indeed best available option in 3D?
      nrdir = if (length(hsa$sgd) == 2) 4L else 9L,
      gridlevel = mhsa$gridlevel |> as.list(),
      mergephasein = nmhsa$mergephasein,
      mergephaseto = nmhsa$mergephaseto |> as.list(),
      targetm = nmhsa$targetm,
      lengthm = nmhsa$lengthm,
      ntm = nmhsa$ntm,
      ti_2 = nmhsa$ti_2,
      split_target = nmhsa$split_target |> as.list(),
      split_targeto = nmhsa$split_targeto,
      rename_target = nmhsa$rename_target,
      lengthsplit = nmhsa$lengthsplit |> as.list(),
      ntsplit = nmhsa$ntsplit |> as.list()
    )
  )
  rui::succeed()
  rec <- rec - hsa$correction
  rec |> as.nmhsa_array()
}
prep_args_nmhsa <- function(
    correction,
    sgd,
    ti,
    ti_merged,
    ti_splitted,
    split_distance,
    split_stop_after,
    merge_pairs,
    merge_distance,
    merge_stop_after
) {
  ti <- ti + correction
  ti_merged <- ti_merged + correction
  ti_splitted <- ti_splitted + correction

  # when splitting was done
  crosstab <- table(c(ti_splitted), c(ti_merged))
  mergephasein <- apply(crosstab != 0, 2, which, simplify = FALSE) |>
    `names<-`(NULL) |>
    lapply(\(x) as.list(`names<-`(x, NULL)))
  mergephaseto <- colnames(crosstab) |> as.integer()
  targetm <- merge_pairs |> lapply(as.integer) |> lapply(as.list)
  if (is.null(merge_distance)) merge_distance <- rep(min(dim(ti), dim(sgd)) %/% 4, length(targetm)) |>
    as.integer()
  lengthm <- merge_distance |>
    check_integer("merge_distance") |>
    check_length(length(merge_pairs), "merge_distance")
  if (is.null(merge_stop_after)) merge_stop_after <- prod(sgd)
  ntm <- merge_stop_after |>
    check_integer("merge_stop_after") |>
    check_length(length(merge_pairs), "merge_stop_after")

  # when merging was done
  ti_2 <- if (is.null(ti_merged)) {
    integer(0)
  } else {
    create_ti_2(
      ti,
      ti_merged |> check_integer("ti_merged")
    )
  }
  split_target <- phases(ti_2)[!phases(ti_2) %in% phases(ti)]
  matches1 <- apply(table(c(ti_merged), c(ti_2)) != 0, 2, which, simplify = FALSE)  |>
    `names<-`(NULL) |>
    lapply(\(x) as.list(`names<-`(x, NULL)))
  # TODO python docs say the order here should be least occurring first ...
  split_targeto <- matrix(c(split_target, matches1[[split_target]]), ncol = 2) |>
    apply(1, as.list, simplify = FALSE)
  matches2 <- apply(table(c(ti), c(ti_2)) != 0, 2, which, simplify = FALSE)  |>
    `names<-`(NULL) |>
    lapply(\(x) as.list(`names<-`(x, NULL)))
  rename_target <- matrix(c(matches2[[split_target]], matches1[[split_target]]), ncol = 2) |>
    apply(1, as.list, simplify = FALSE)
  lengthsplit <- split_distance
  if (is.null(lengthsplit)) lengthsplit <- rep(min(dim(ti), dim(sgd)) %/% 4, length(split_target)) |>
    as.integer()
  if (length(lengthsplit) < length(split_target)) lengthsplit <- rep(lengthsplit, length(split_target))
  lengthsplit <- lengthsplit |> check_integer("split_distance")
  if (is.null(split_stop_after)) split_stop_after <- prod(sgd)
  ntsplit <- split_stop_after |> check_integer("split_stop_after")
  if (length(ntsplit) < length(split_target)) ntsplit <- rep(ntsplit, length(split_target))

  list(tiini = ti_splitted,
       ti_2 = ti_2,
       mergephasein = mergephasein,
       mergephaseto = mergephaseto,
       targetm = targetm,
       lengthm = lengthm,
       ntm = ntm,
       split_target = split_target,
       split_targeto = split_targeto,
       rename_target = rename_target,
       lengthsplit = lengthsplit,
       ntsplit = ntsplit)
}
create_ti_2 <- function(ti, ti_merged) {
  ti_2 <- ti
  differences <- which(ti != ti_merged)
  tab <- table(ti_merged[differences], ti[differences]) != 0
  for (j in 1:ncol(tab)) {
    for (i in 1:nrow(tab)) {
      if (tab[i, j]) {
        newlabel <- max(ti_2) + 1
        ti_2[which(
          ti_merged == as.integer(rownames(tab)[i]) &
            ti == as.integer(colnames(tab)[j])
        )] <- newlabel
      }
    }
  }
  ti_2
}

# class nmhsa_array ----

#' @export
print.nmhsa_array <- function(...) {
  rui::inspect(...)
}

#' @export
as.nmhsa_array <- function(x) {
  if ("nmhsa_array" %in% class(x)) return(x)
  class(x) <- c("nmhsa_array", class(x))
  x
}

#' @export
plot.nmhsa_array <- function(x, y = NULL, i, j, k) {
  if (length(dim(x)) == 3) {
    if (!missing(i)) x <- x[i,,]
    if (!missing(j)) x <- x[,j,]
    if (!missing(k)) x <- x[,,k]
    if (missing(i) & missing(j) & missing(k)) {
      x <- x[,,1]
      rui::alert("Plotting 3D image without subsetting")
      rui::inform("Taking first layer (k = 1)")
    }
  }
  if (length(dim(y)) == 3) {
    if (!missing(i)) y <- y[i,,]
    if (!missing(j)) y <- y[,j,]
    if (!missing(k)) y <- y[,,k]
    if (missing(i) & missing(j) & missing(k)) {
      y <- y[,,1]
      rui::alert("Plotting 3D image without subsetting")
      rui::inform("Taking first layer (k = 1)")
    }
  }
  if (is.null(y)) img(x) else imgs(x, y)
  invisible()
}
img <- function(x, legend = TRUE) {
  phases <- sort(unique(c(x)))
  nphases <- max(phases)
  if (legend) {
    op <- par(mar=rep(0.5, 4))
    layout(matrix(c(1, 2), ncol = 1), heights = c(.85, .15))
    on.exit({par(op); layout(1)})
  }
  image(
    x,
    xaxt = "n",
    yaxt = "n",
    col = palette.colors(nphases)[phases],
    useRaster = TRUE,
    asp = nrow(x)/ncol(x),
    bty = "n"
  )
  if (legend) {
    plot(1, type = "n", axes = FALSE, xlab="", ylab="")
    legend(
      "top",
      legend = 1:nphases,
      col = palette.colors(nphases),
      pch = 15,
      horiz = TRUE,
      bty = "n",
      cex = 1.5
    )
  }
}
imgs <- function(x, y) {
  phases <- sort(unique(c(x, y)))
  nphases <- max(phases)
  op <- par(mar = rep(0.5, 4))
  layout(matrix(c(1,3,2,3), ncol = 2), heights = c(0.85, 0.15))
  on.exit({par(op); layout(1)})
  img(x, legend = FALSE)
  img(y, legend = FALSE)
  plot(1, type = "n", axes = FALSE, xlab="", ylab="")
  legend(
    "top",
    legend = 1:nphases,
    col = palette.colors(nphases)[phases],
    pch = 15,
    horiz = TRUE,
    bty = "n",
    cex = 1.5
  )
}
