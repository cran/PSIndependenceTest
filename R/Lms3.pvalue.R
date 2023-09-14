#' @title Logarithmic minimum test for independence in three-way contingency table
#'
#' @description
#' Calculates the p-value of the Logarithmic minimum test for independence in three-way contingency table
#'
#' @param stat a Logarithmic minimum statistic value
#' @param nr a number of rows
#' @param nc a number of columns
#' @param nt a number of tubes
#' @param n a sample size
#' @param B an integer specifying the number of replicates used in the Monte Carlo test (optional)
#' @return The function returns the p-value of the logarithmic minimum test.
#' @rdname Lms3.pvalue
#'
#' @details
#' The Critical value of the modular test for independence in r x c x t contingency table,
#'
#' @author
#' Piotr Sulewski, \email{piotr.sulewski@apsl.edu.pl}, Pomeranian University in Slupsk.
#'
#' @references
#' {Sulewski, P. (2021). \emph{Logarithmic Minimum Test for Independence in Three Way Con-tingency Table of Small Sizes,} Journal of Statistical Computation and Simulation 91(13), 2780-2799}
#'
#' @examples
#' tab1 = GenTab3(array(0.125, dim = c(2, 2, 2)), 100)
#' Lms3.pvalue(Lms3.stat(tab1), 2, 2, 2, 100, B=1e2)
#' Lms3.pvalue(Lms3.stat(table4), 2, 2, 2, 80, B = 1e3)
#'
#' @export

Lms3.pvalue <- function(stat, nr, nc, nt, n, B = 1e4) {
  Q <- numeric(B)
  CDF <- numeric(B+1)
  for (i in 1:B) CDF[i] <- i / (B + 1)
  CDF[B+1] = 1
  pijt <- array(1 / (nr * nc * nt), dim = c(nr, nc, nt))
  for (u in 1:B){
    nijt <- GenTab3(pijt, n)
    Q[u] <- Lms3.stat(nijt)
  }
  Q <- sort(Q)
  poz <- B + 1
  for (i in B:1) {
    if (Q[i] > stat) poz = i else break
  }
  return(1 - CDF[poz])
}
