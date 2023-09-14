#' @title Logarithmic minimum test for independence in three-way contingency table
#'
#' @description
#' Calculates the test statistic and p-value of the Logarithmic minimum  test for independence in three-way contingency table
#'
#' @param nijt a numeric matrix with non-negative values of the three-way contingency table cells
#' @param B an integer specifying the number of replicates used in the Monte Carlo test (optional)
#' @return The function returns values of the test statistic and p-value of the logarithmic minimum test.
#' @rdname Lms3.test
#'
#' @details
#' The test statistic and p-value of the Logarithmic minimum  test for independence in r x c x t contingency table,
#'
#' @author
#' Piotr Sulewski, \email{piotr.sulewski@apsl.edu.pl}, Pomeranian University in Slupsk.
#'
#' @references
#' {Sulewski, P. (2021). \emph{Logarithmic Minimum Test for Independence in Three Way Con-tingency Table of Small Sizes,} Journal of Statistical Computation and Simulation 91(13), 2780-2799}
#'
#' @examples
#' \donttest{Lms3.test(GenTab3(array(0.125, dim = c(2, 2, 2)), 80), B = 1e2)}
#' \donttest{Lms3.test(table4, B = 1e3)}
#'
#' @export

Lms3.test <- function(nijt, B = 1e4) {
  nr <- dim(nijt)[1]
  nc <- dim(nijt)[2]
  nt <- dim(nijt)[3]
  NameD <- deparse(substitute(nijt))
  stat <- Lms3.stat(nijt)
  RVal <- list(statistic = c(D =stat),
        p.value = Lms3.pvalue(stat, nr, nc, nt, sum(nijt), B = 1e4),
        method = "Logarithmic minimum test for independence in three-way contingency table", data.name = NameD)
  class(RVal) <- "htest"
  return(RVal)
}
