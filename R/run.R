#' Run benchmarks
#'
#' @param n An integer with the number of times each benchmark will be run.
#' @param which A vector of characters with the codes of the tests to run, or
#'   NULL to run them all.
#'
#' @details
#'   **B01** Create, transpose, deform a 2500x2500 matrix
#'   **B02** Raise a 2400x2400 matrix to 1000th power
#'   **B03** Sort 7,000,000 random values
#'   **B04** Crossproduct of 2800x2800 matrix (b = a' * a)
#'   **B05** Solve linear regression over a 3000x3000 matrix (c = a \\ b')
#'   **B06** FFT over 2,400,000 random values
#'   **B07** Eigenvalues of a 640x640 random matrix
#'   **B08** Determinant of a 2500x2500 random matrix
#'   **B09** Cholesky decomposition of a 3000x3000 matrix
#'   **B10** Inverse of a 1600x1600 random matrix
#'   **B11** Find 3,500,000 Fibonacci numbers (vector calculation)
#'   **B12** Create a 3000x3000 Hilbert matrix (matrix calculation)
#'   **B13** Grand common divisors of 400,000 pairs (recursion)
#'   **C01** QR decomposition
#'   **C02** SVD decomposition
#'   **C03** LU decomposition
#'
#' @return Print the summary, and also writes to disk the timing data frame and
#'   the summary itself.
#' @export
#'
#' @examples
#' \dontrun{
#'   run(1000, NULL)
#' }
run <- function(n = 100, which = NULL) {
  ops <- c("b01", "b02", "b03", "b04", "b05", "b06", "b07", "b08", "b09",
           "b10", "b11", "b12", "b13", "b14", "b15", "c1", "c2", "c3")

  if (is.null(which))
    which <- ops

  if (is.integer(n) | length(unique(n)) != 1)
    stop("`n` can be one integer only")

  if (!all(which %in% ops))
    stop("Invalid option in `which`")

  # Create a queue in random order
  queue <- sample(rep(which, n))

  # Run the benchmarks
  l <- pbapply::pblapply(queue, function(fname){ get(fname)() })

  # Produce the output data frame
  res <- data.frame(
    f = queue,
    do.call(rbind, l)
  )

  # Out you go!
  saveRDS(res, "runs.RDS")
  capture.output({
    # Sysinfo
    cat("System info\n")
    print(sessionInfo())

    # Legend
    cat("\nBenchmark codes\n")
    cat("  B01 Create, transpose, deform a 2500x2500 matrix\n")
    cat("  B02 Raise a 2400x2400 matrix to 1000th power\n")
    cat("  B03 Sort 7,000,000 random values\n")
    cat("  B04 Crossproduct of 2800x2800 matrix (b = a' * a)\n")
    cat("  B05 Solve linear regression over a 3000x3000 matrix (c = a \\ b')\n")
    cat("  B06 FFT over 2,400,000 random values\n")
    cat("  B07 Eigenvalues of a 640x640 random matrix\n")
    cat("  B08 Determinant of a 2500x2500 random matrix\n")
    cat("  B09 Cholesky decomposition of a 3000x3000 matrix\n")
    cat("  B10 Inverse of a 1600x1600 random matrix\n")
    cat("  B11 Find 3,500,000 Fibonacci numbers (vector calculation)\n")
    cat("  B12 Create a 3000x3000 Hilbert matrix (matrix calculation)\n")
    cat("  B13 Grand common divisors of 400,000 pairs (recursion)\n")
    cat("  C01 QR decomposition\n")
    cat("  C02 SVD decomposition\n")
    cat("  C03 LU decomposition\n")

    # Number of runs
    cat(sprintf("\nEach benchmark was run %d times.\n", n))

    # Run summary
    for (i in 2:4) {
      cat(sprintf("\nTiming summary for %s\n", colnames(res)[i]))
      print(do.call(rbind, tapply(res[, 2], as.factor(res$f), summary)))
    }
  }, file = "summary.txt")

  cat(paste(readLines(file("summary.txt")), collapse = "\n"))

  cat("\n")
}
