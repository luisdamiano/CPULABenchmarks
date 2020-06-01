#'@import Matrix
#'@importFrom methods new
#'@importFrom stats cor fft rnorm runif
#'@importFrom utils capture.output sessionInfo

# From r-with-intel-mkl ---------------------------------------------------
# https://github.com/pachamaltese/r-with-intel-mkl/blob/master/00-benchmark-scripts/1-r-benchmark-25.R

# I. Matrix calculation ---------------------------------------------------
# (1)
# Creation, transposition, deformation of a 2500x2500 matrix --------------------
b01 <- function() {
  invisible(gc())

  system.time({
    a <- matrix(rnorm(2500 * 2500) / 10, ncol = 2500, nrow = 2500)
    b <- t(a)
    dim(b) <- c(1250, 5000)
    a <- t(b)
  })
}

# (2)
# cat(c("2400x2400 normal distributed random matrix ^1000____ (sec): ", timing, "\n"))
b02 <- function() {
  a <- abs(matrix(rnorm(2500 * 2500) / 2, ncol = 2500, nrow = 2500))
  b <- 0

  invisible(gc())
  system.time({
    b <- a ^ 1000
  })
}

# (3)
# cat(c("Sorting of 7,000,000 random values__________________ (sec): ", timing, "\n"))
b03 <- function() {
  a <- rnorm(7000000)
  b <- 0

  invisible(gc())
  system.time({
    b <- sort(a, method = "quick")
  })
}

# (4)
# cat(c("2800x2800 cross-product matrix (b = a' * a)_________ (sec): ", timing, "\n"))
b04 <- function() {
  a <- rnorm(2800 * 2800)
  dim(a) <- c(2800, 2800)
  b <- 0

  invisible(gc())
  system.time({
    b <- crossprod(a) # equivalent to: b <- t(a) %*% a
  })
}

# (5)
# cat(c("Linear regr. over a 3000x3000 matrix (c = a \\ b')___ (sec): ", timing, "\n"))
b05 <- function() {
  a <-
    new("dgeMatrix", x = rnorm(2000 * 2000), Dim = as.integer(c(2000, 2000)))
  b <- as.double(1:2000)

  invisible(gc())
  system.time({
    c <- solve(crossprod(a), crossprod(a, b))
  })
}

# II. Matrix functions ----------------------------------------------------
# (1)
# cat(c("FFT over 2,400,000 random values____________________ (sec): ", timing, "\n"))
b06 <- function() {
  a <- rnorm(2400000)
  b <- 0
  invisible(gc())
  system.time({
    b <- fft(a)
  })
}

# (2)
# cat(c("Eigenvalues of a 640x640 random matrix______________ (sec): ", timing, "\n"))
b07 <- function() {
  a <- array(rnorm(600 * 600), dim = c(600, 600))
  b <- 0

  invisible(gc())
  system.time({
    b <- eigen(a, symmetric = FALSE, only.values = TRUE)$Value
  })
}

# (3)
# cat(c("Determinant of a 2500x2500 random matrix____________ (sec): ", timing, "\n"))
b08 <- function() {
  a <- rnorm(2500 * 2500)
  dim(a) <- c(2500, 2500)
  b <- 0

  invisible(gc())
  system.time({
    b <- det(a)
  })
}

# (4)
# cat(c("Cholesky decomposition of a 3000x3000 matrix________ (sec): ", timing, "\n"))
b09 <- function() {
  a <- crossprod(new("dgeMatrix", x = rnorm(3000 * 3000),
                     Dim = as.integer(c(3000, 3000))))
  b <- 0

  invisible(gc())
  system.time({
    b <- chol(a)
  })
}

# (5)
# cat(c("Inverse of a 1600x1600 random matrix________________ (sec): ", timing, "\n"))
b10 <- function() {
  a <-
    new("dgeMatrix", x = rnorm(1600 * 1600), Dim = as.integer(c(1600, 1600)))
  b <- 0

  invisible(gc())
  system.time({
    b <- solve(a)
  })
}

# III. Programmation ------------------------------------------------------
# (1)
# cat(c("3,500,000 Fibonacci numbers calculation (vector calc)(sec): ", timing, "\n"))
b11 <- function() {
  a <- 0
  b <- 0
  phi <- 1.6180339887498949
  a <- floor(runif(3500000) * 1000)

  invisible(gc())
  system.time({
    b <- (phi ^ a - (-phi) ^ (-a)) / sqrt(5)
  })
}

# (2)
# cat(c("Creation of a 3000x3000 Hilbert matrix (matrix calc) (sec): ", timing, "\n"))

b12 <- function() {
  a <- 3000
  b <- 0

  invisible(gc())
  system.time({
    b <- rep(1:a, a)
    dim(b) <- c(a, a)

    b <- 1 / (t(b) + 0:(a - 1))
  })
}

# (3)
# cat(c("Grand common divisors of 400,000 pairs (recursion)__ (sec): ", timing, "\n"))
b13 <- function() {
  a <- ceiling(runif(400000) * 1000)
  b <- ceiling(runif(400000) * 1000)
  c <- 0
  gcd2 <-
    function(x, y) {
      if (sum(y > 1.0E-4) == 0)
        x
      else {
        y[y == 0] <- x[y == 0]
        Recall(y, x %% y)
      }
    }

  invisible(gc())
  system.time({
    c <- gcd2(a, b) # gcd2 is a recursive function
  })
}

# (4)
# cat(c("Creation of a 500x500 Toeplitz matrix (loops)_______ (sec): ", timing, "\n"))
b14 <- function() {
  b <- rep(0, 500 * 500)
  dim(b) <- c(500, 500)

  invisible(gc())
  system.time({
    # Rem: there are faster ways to do this
    # but here we want to time loops (220*220 'for' loops)!
    for (j in 1:500) {
      for (k in 1:500) {
        b[k, j] <- abs(j - k) + 1
      }
    }
  })
}

# (5)
# cat(c("Escoufier's method on a 45x45 matrix (mixed)________ (sec): ", timing, "\n"))
b15 <- function() {
  p <- 0; vt <- 0; vr <- 0; vrt <- 0; rvt <- 0; RV <- 0; j <- 0; k <- 0;
  x2 <- 0; R <- 0; Rxx <- 0; Ryy <- 0; Rxy <- 0; Ryx <- 0; Rvmax <- 0
  Trace <- function(y) {
    sum(c(y)[1 + 0:(min(dim(y)) - 1) * (dim(y)[1] + 1)], na.rm = FALSE)
  }
  x  <- abs(rnorm(45*45)); dim(x) <- c(45, 45)

  invisible(gc())
  system.time({
    # Calculation of Escoufier's equivalent vectors
    p <- ncol(x)
    vt <- 1:p                                  # Variables to test
    vr <- NULL                                 # Result: ordered variables
    RV <- 1:p                                  # Result: correlations
    vrt <- NULL
    for (j in 1:p) {
      # loop on the variable number
      Rvmax <- 0
      for (k in 1:(p - j + 1)) {
        # loop on the variables
        x2 <- cbind(x, x[, vr], x[, vt[k]])
        R <- cor(x2)                           # Correlations table
        Ryy <- R[1:p, 1:p]
        Rxx <- R[(p + 1):(p + j), (p + 1):(p + j)]
        Rxy <- R[(p + 1):(p + j), 1:p]
        Ryx <- t(Rxy)
        rvt <- # RV calculation
          Trace(Ryx %*% Rxy) / sqrt(Trace(Ryy %*% Ryy) * Trace(Rxx %*% Rxx))
        if (rvt > Rvmax) {
          Rvmax <- rvt  # test of RV
          vrt <-
            vt[k]       # temporary held variable
        }
      }
      vr[j] <- vrt      # Result: variable
      RV[j] <- Rvmax    # Result: correlation
      vt <-
        vt[vt != vr[j]] # reidentify variables to test
    }
  })
}

# gcbd --------------------------------------------------------------------
# https://github.com/cran/gcbd/blob/master/R/benchmark.R
c1 <- function() {
  a <- rnorm(2800 * 2800)
  dim(a) <- c(2800, 2800)
  b <- 0

  invisible(gc())
  system.time({
    b <- qr(a, LAPACK = TRUE)
  })
}

c2 <- function() {
  a <- rnorm(2800 * 2800)
  dim(a) <- c(2800, 2800)
  b <- 0

  invisible(gc())
  system.time({
    b <- svd(a)
  })
}

c3 <- function() {
  a <- rnorm(2800 * 2800)
  dim(a) <- c(2800, 2800)
  b <- 0

  invisible(gc())
  system.time({
    b <- lu(a)
  })
}
