#' @title Modular test for independence in four-way contingency table
#'
#' @description
#' Calculates the critical value of the modular test for independence in four-way contingency table
#'
#' @param nr a number of rows
#' @param nc a number of columns
#' @param nt a number of tubes
#' @param nu a number of tubes
#' @param n a sample size
#' @param alfa a significance level
#' @param B an integer specifying the number of replicates used in the Monte Carlo test (optional)
#' @return The function returns the critical value of the modular test.
#' @rdname Mod4.cv
#'
#' @details
#' The Critical value of the Logarithmic minimum test for independence in r x c x t contingency table,
#'
#' @author
#' Piotr Sulewski, \email{piotr.sulewski@apsl.edu.pl}, Pomeranian University in Slupsk.
#'
#' @references
#' Extension of the information contained in {Sulewski, P. (2018). \emph{Power Analysis Of Independence Testing for the Three-Way Con-tingency Tables of Small Sizes.} Journal of Applied Statistics 45(13), 2481-2498}
#'
#' @examples
#'
#' Mod4.cv(2, 2, 2, 2, 160, 0.05, B = 1e2)
#' \donttest{Mod4.cv(2, 2, 2, 2, 160, 0.1, B = 1e3)}
#'
#' @export

Mod4.cv <- function(nr, nc, nt, nu, n, alfa, B = 1e4) {
  Q <- numeric(B)
  pijt <- array(1 / (nr * nc * nt * nu), dim = c(nr, nc, nt, nu))
  for (u in 1:B){
    nijtu <- GenTab4(pijt, n)
    Q[u] <- Mod4.stat(nijtu)
  }
  Q <- sort(Q)
  return(Q[(1 - alfa) * B])
}
