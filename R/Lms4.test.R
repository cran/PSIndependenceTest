#' @title Logarithmic minimum test for independence in four-way contingency table
#'
#' @description
#' Calculates the test statistic and p-value of the Logarithmic minimum  test for independence in four-way contingency table
#'
#' @param nijtu a numeric matrix with non-negative values of the four-way contingency table cells
#' @param B an integer specifying the number of replicates used in the Monte Carlo test (optional)
#' @return The function returns values of the test statistic and p-value of the logarithmic minimum test.
#' @rdname Lms4.test
#'
#' @details
#' The test statistic and p-value of the Logarithmic minimum  test for independence in r x c x t x u contingency table,
#'
#' @author
#' Piotr Sulewski, \email{piotr.sulewski@apsl.edu.pl}, Pomeranian University in Slupsk.
#'
#' @references
#' Extension of the information contained in {Sulewski, P. (2021). \emph{Logarithmic Minimum Test for Independence in Three Way Con-tingency Table of Small Sizes,} Journal of Statistical Computation and Simulation 91(13), 2780-2799}
#'
#' @examples
#' \donttest{Lms4.test(GenTab4(array(1/16, dim = c(2, 2, 2, 2)), 160), B = 1e2)}
#' \donttest{Lms4.test(table6, B = 1e3)}
#'
#' @export

Lms4.test <- function(nijtu, B = 1e4) {
  nr <- dim(nijtu)[1]
  nc <- dim(nijtu)[2]
  nt <- dim(nijtu)[3]
  nu <- dim(nijtu)[4]
  NameD <- deparse(substitute(nijtu))
  stat <- Lms4.stat(nijtu)
  RVal <- list(statistic = c(D =stat),
        p.value = Lms4.pvalue(stat, nr, nc, nt, nu, sum(nijtu), B = 1e4),
        method = "Logarithmic minimum test for independence in four-way contingency table", data.name = NameD)
  class(RVal) <- "htest"
  return(RVal)
}
