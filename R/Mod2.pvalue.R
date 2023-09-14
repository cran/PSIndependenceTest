#' @title Modular test for independence in two-way contingency table
#'
#' @description
#' Calculates the p-value of the modular test for independence in two-way contingency table
#'
#' @param stat a modular statistic value
#' @param nr a number of rows
#' @param nc a number of columns
#' @param n a sample size
#' @param B an integer specifying the number of replicates used in the Monte Carlo test (optional)
#' @return The function returns the p-value of the modular test.
#' @rdname Mod2.pvalue
#'
#' @details
#' The p-value of the modular test for independence in r x c contingency table,
#'
#' @author
#' Piotr Sulewski, \email{piotr.sulewski@apsl.edu.pl}, Pomeranian University in Slupsk.
#'
#' @references
#' {Sulewski, P. (2016). \emph{Moc testów niezależności w tablicy dwudzielczej większej niż 2×2,} Przegląd statystyczny 63(2), 190-210}
#'
#' @examples
#' pij=matrix(1/4, nrow = 2, ncol = 2)
#' tab4=GenTab2(pij, 30)
#' Mod2.pvalue(Mod2.stat(tab4), 2, 2, 30, B=1e3)
#' \donttest{Mod2.pvalue(2.5, 3, 2, 60)}
#'
#' @export

Mod2.pvalue <- function(stat, nr, nc, n, B = 1e4) {
  Q <- numeric(B)
  CDF <- numeric(B+1)
  for (i in 1:B) CDF[i] <- i / (B + 1)
  CDF[B+1] = 1
  pij <- array(1 / (nr * nc), dim = c(nr, nc))
  for (u in 1:B){
    nij <- GenTab2(pij, n)
    Q[u] <- Mod2.stat(nij)
  }
  Q <- sort(Q)
  poz <- B+1
  for (i in B:1) {
    if (Q[i] > stat) poz = i else break
  }
  return(1 - CDF[poz])
}
