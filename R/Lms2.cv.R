#' @title Logarithmic Minimum Test for Independence in Two-Way Contingency Table
#'
#' @description
#' Calculates the critical values of the logarithmic minimum test.
#'
#' @param nr a number of rows
#' @param nc a number of columns
#' @param n a sample size
#' @param alfa a significance level
#' @param B an integer specifying the number of replicates used in the Monte Carlo test (optional)
#' @return The function returns the critical value of the logarithmic minimum test.
#' @rdname Lms2.cv
#'
#' @details
#' The Critical value of the logarithmic minimum test for independence in r x c contingency table,
#'
#' @author
#' Piotr Sulewski, \email{piotr.sulewski@apsl.edu.pl}, Pomeranian University in Slupsk.
#'
#' @references
#' {Sulewski, P. (2019). \emph{The LMS for Testing Independence in Two-way Contingency Tables.} Biometrical Letters 56(1), 17-43} #'
#'
#' @examples
#' \donttest{Lms2.cv(2, 2, 40, 0.05, B = 1e3)}
#' Lms2.cv(2, 3, 60, 0.1, B = 1e2)
#'
#' @export

Lms2.cv <- function(nr, nc, n, alfa, B=1e4) {
  Q <- numeric(B)
  pij <- array(1 / (nr * nc), dim = c(nr, nc))
  for (u in 1:B){
    nij <- GenTab2(pij, n)
    Q[u] <- Lms2.stat(nij)
  }
  Q <- sort(Q)
  return(Q[(1 - alfa) * B])
}

