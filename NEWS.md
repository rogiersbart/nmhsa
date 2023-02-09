# nmhsa (development version)

* New `nmhsa_array` S3 class enables quicker plotting and more useful
  printing, mainly intended for the docs.
* New `phase_merge()` and `phase_split()` expose the functionality of
  `py$hole_removement()` and `py$phase_split()`.
* New `hsa()`, `mhsa()` and `nmhsa()` expose the different operation modes of
  `py$NMH_SA()` and `py$NMH_SA2Dto3D()`.
* Adjustments to the Python code
    * New {rui} Python function calls replace original Python messages
    * Code is reformatted for improved readability
* Bug fixes in the Python code
    * `ordernew` is used for the final phase label in the 3D case instead of
      `order`, to relabel correctly afterwards from `ordernew` to `order`
    * `sg` is used in an instance of `sgini0` where it was not available.
    * `annstep3d()` is used consistently instead of `annstep()` in the 3D case.
    * The first, simpler, 3D example from the wrapper is removed as it was
      overwritten by the second example anyway.
    * Deprecated `np.int()` was replaced by `int()`.
* Added a `NEWS.md` file to track changes to the package.
