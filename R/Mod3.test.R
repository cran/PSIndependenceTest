#' @title Modular test for independence in three-way contingency table
#'
#' @description
#' Calculates the test statistic and p-value of the modular test for independence in three-way contingency table
#'
#' @param nijt a numeric matrix with non-negative values of the three-way contingency table cells
#' @param B an integer specifying the number of replicates used in the Monte Carlo test (optional)
#' @return The function returns values of the test statistic and p-value of the modular test.
#' @rdname Mod3.test
#'
#' @details
#' The test statistic and p-value of the modular test for independence in r x c x t contingency table,
#'
#' @author
#' Piotr Sulewski, \email{piotr.sulewski@apsl.edu.pl}, Pomeranian University in Slupsk.
#'
#' @references
#' {Sulewski, P. (2018). \emph{Power Analysis Of Independence Testing for the Three-Way Con-tingency Tables of Small Sizes.} Journal of Applied Statistics 45(13), 2481-2498}
#'
#' @examples
#' \donttest{Mod3.test(GenTab3(array(0.125, dim = c(2, 2, 2)), 80), B = 1e3)}
#' \donttest{Mod3.test(table4, B = 1e3)}
#'
#' @export


Mod3.test <- function(nijt, B = 1e4) {
  nr <- dim(nijt)[1]
  nc <- dim(nijt)[2]
  nt <- dim(nijt)[3]
  NameD <- deparse(substitute(nijt))
  stat <- Mod3.stat(nijt)
  RVal <- list(statistic = c(D = stat),
          p.value = Mod3.pvalue(stat, nr, nc, nt, sum(nijt), B = 1e4),
          method = "Modular test for independence in three-way contingency table", data.name = NameD)
  class(RVal) <- "htest"
  return(RVal)
}




