#' @title Logarithmic Minimum Test for Independence in Two-Way Contingency Table
#'
#' @description
#' Calculates the p-value of the logarithmic minimum test.
#'
#' @param stat a logarithmic minimum statistic value
#' @param nr a number of rows
#' @param nc a number of columns
#' @param n a sample size
#' @param B an integer specifying the number of replicates used in the Monte Carlo test (optional)
#' @return The function returns the p-value of the logarithmic minimum test.
#' @rdname Lms2.pvalue
#'
#' @details
#' The p-value of  the logarithmic minimum test for independence in r x c contingency table,
#'data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABIAAAASCAYAAABWzo5XAAAAWElEQVR42mNgGPTAxsZmJsVqQApgmGw1yApwKcQiT7phRBuCzzCSDSHGMKINIeDNmWQlA2IigKJwIssQkHdINgxfmBBtGDEBS3KCxBc7pMQgMYE5c/AXPwAwSX4lV3pTWwAAAABJRU5ErkJggg==
#' @author
#' Piotr Sulewski, \email{piotr.sulewski@apsl.edu.pl}, Pomeranian University in Slupsk.
#'
#' @references
#' {Sulewski, P. (2019). \emph{The LMS for Testing Independence in Two-way Contingency Tables.} Biometrical Letters 56(1), 17-43}
#'
#' @examples
#' Lms2.pvalue(Lms2.stat(table1), 2, 2, 40, B = 1e3)
#' Lms2.pvalue(Lms2.stat(table2), 2, 3, 60, B = 1e2)
#'
#' @export

Lms2.pvalue <- function(stat, nr, nc, n, B = 1e4) {
  Q <- numeric(B)
  CDF <- numeric(B+1)
  for (i in 1:B) CDF[i] <- i / (B + 1)
  CDF[B+1] = 1
  pij <- array(1 / (nr * nc), dim = c(nr, nc))
  for (u in 1:B){
    nij <- GenTab2(pij, n)
    Q[u] <- Lms2.stat(nij)
  }
  Q <- sort(Q)
  poz <- B+1
  for (i in B:1) {
    if (Q[i] > stat) poz = i else break
  }
  return(1 - CDF[poz])
}
