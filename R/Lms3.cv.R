#' @title Logarithmic minimum test for independence in three-way contingency table
#'
#' @description
#' Calculates the critical value of the Logarithmic minimum test for independence in three-way contingency table
#' (see Sulewski P. (2018)).
#'
#' @param nr a number of rows
#' @param nc a number of columns
#' @param nt a number of tubes
#' @param n a sample size
#' @param alfa a significance level
#' @param B an integer specifying the number of replicates used in the Monte Carlo test (optional)
#' @return The function returns the critical value of the logarithmic minimum test.
#' @rdname Lms3.cv
#'
#' @details
#' The Critical value of the Logarithmic minimum test for independence in r x c x t contingency table,
#'
#' @author
#' Piotr Sulewski, \email{piotr.sulewski@apsl.edu.pl}, Pomeranian University in Slupsk.
#'
#' @references
#' {Sulewski, P. (2021). \emph{Logarithmic Minimum Test for Independence in Three Way Con-tingency Table of Small Sizes,} Journal of Statistical Computation and Simulation 91(13), 2780-2799}
#'
#' @examples
#' Lms3.cv(2, 2, 2, 80, 0.05, B = 1e2)
#' \donttest{Lms3.cv(2, 2, 2, 80, 0.1, B = 1e3)}
#'
#' @export

Lms3.cv <- function(nr, nc, nt, n, alfa, B = 1e4) {
  Q <- numeric(B)
  pijt <- array(1 / (nr * nc * nt), dim = c(nr, nc, nt))
  for (u in 1:B){
    nijt <- GenTab3(pijt, n)
    Q[u] <- Lms3.stat(nijt)
  }
  Q <- sort(Q)
  return(Q[(1 - alfa) * B])
}
