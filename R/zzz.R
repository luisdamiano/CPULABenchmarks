#' @details
#' A one-line function to run 18 basic benchmarks for linear algebra
#' operations run on the CPU. Benchmarks follow Mauricio Vargas Sepulveda's
#'     `r-with-intel-mkl` GitHub repository and Dirk Eddelbuettel's `gcbd`
#' R package.
#' @keywords internal
"_PACKAGE"

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    sprintf(
      "[CPULABenchmarks %s] Do `?run`, or just try `run(5)` to get a feeling.",
      utils::packageDescription("CPULABenchmarks")$Version
    )
  )

  options(object.size = 100000000)
}
