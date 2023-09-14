#' @title Two-way contingency table r x c  - generation
#'
#' @description
#' Generating a two-way contingency table r x c
#'
#' @param pij a numeric matrix with non-negative probability values of the two-way contingency table
#' @param n a sample size
#' @return The function returns the two-way contingency table r x c
#' @rdname GenTab2
#'
#' @details
#' Generating a two-way contingency table r x c using the probability matrix pij.
#' If Ho is true then pij equals 1 / r / c.
#'
#' @author
#' Piotr Sulewski, \email{piotr.sulewski@apsl.edu.pl}, Pomeranian University in Slupsk.
#'
#' @references
#' {Sulewski, P. (2016). \emph{Moc testów niezależności w tablicy dwudzielczej większej niż 2×2,} Przegląd statystyczny 63(2), 190-210}
#'
#' @examples
#' r = 6; c = 2
#' GenTab2(array(1 / r / c, dim = c(r, c)), 93)
#' GenTab2(matrix(c(0.125,0.25,0.25,0.375), nrow=2), 100)
#'
#' @export

GenTab2 <- function(pij, n) {
  nr <- nrow(pij)
  nc <- ncol(pij)
  SumProb <- numeric(nr * nc)
  nij <- array(0, c(nr, nc))
  u <- 1
  SumProb[1] <- 0
  for(i in 1:nr) {
    for(j in 1:nc) {
      u <- u + 1
      SumProb[u] <- SumProb[u-1] + pij[i,j]
    }
  }
  for (d in 1:n) {
    los <- stats::runif(1,0,1)
    u <- 1
    for(i in 1:nr) {
      for(j in 1:nc) {
        u <- u + 1
        if(los > SumProb[u - 1] & los <= SumProb[u]) nij[i,j] <- nij[i,j] + 1
      }
    }
  }
  return (nij)
}

