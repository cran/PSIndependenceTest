#' @title Logarithmic Minimum Test for Independence in Two-Way Contingency Table
#'
#' @description
#' Calculates the test statistic and p-value of the logarithmic minimum test.
#'
#' @param nij a numeric matrix with non-negative values of the two-way contingency table cells
#' @param B an integer specifying the number of replicates used in the Monte Carlo test (optional)
#' @return The function returns values of the test statistic and p-value of the logarithmic minimum test.
#' @rdname Lms2.test
#'
#' @details
#' The test statistic and p-value of he logarithmic minimum test for independence in r x c contingency table,
#'
#' @author
#' Piotr Sulewski, \email{piotr.sulewski@apsl.edu.pl}, Pomeranian University in Slupsk.
#'
#' @references
#' {Sulewski, P. (2019). \emph{The LMS for Testing Independence in Two-way Contingency Tables.} Biometrical Letters 56(1), 17-43}
#'
#' @examples
#' \donttest{Lms2.test(GenTab2(matrix(1/6, nrow = 2, ncol = 3), 50), B = 1e2)}
#' \donttest{Lms2.test(table2, B = 1e3)}
#'
#' @export

Lms2.test <- function(nij, B = 1e4) {
  nr <- nrow(nij)
  nc <- ncol(nij)
  NameD <- deparse(substitute(nij))
  stat <- Lms2.stat(nij)
  RVal <- list(statistic = c(D = stat),
               p.value = Lms2.pvalue(stat, nr, nc, sum(nij), B = 1e4),
               method = "Modular test for independence in two-way contingency table",
               data.name = NameD)
  class(RVal) <- "htest"
  return(RVal)
}




