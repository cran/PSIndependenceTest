#' @title Modular test for independence in four-way contingency table
#'
#' @description
#' Calculates the test statistic and p-value of the modular test for independence in four-way contingency table
#'
#' @param nijtu a numeric matrix with non-negative values of the four-way contingency table cells
#' @param B an integer specifying the number of replicates used in the Monte Carlo test (optional)
#' @return The function returns values of the test statistic and p-value of the modular test.
#' @rdname Mod4.test
#'
#' @details
#' The test statistic and p-value of the modular test for independence in r x c x t x u contingency table,
#'
#' @author
#' Piotr Sulewski, \email{piotr.sulewski@apsl.edu.pl}, Pomeranian University in Slupsk.
#'
#' @references
#' Extension of the information contained in {Sulewski, P. (2018). \emph{Power Analysis Of Independence Testing for the Three-Way Con-tingency Tables of Small Sizes.} Journal of Applied Statistics 45(13), 2481-2498}
#'
#' @examples
#' \donttest{Mod4.test(GenTab4(array(1/16, dim = c(2, 2, 2, 2)), 160), B = 1e2)}
#' \donttest{Mod4.test(table6, B = 1e2)}
#'
#' @export

Mod4.test <- function(nijtu, B = 1e4) {
  nr <- dim(nijtu)[1]
  nc <- dim(nijtu)[2]
  nt <- dim(nijtu)[3]
  nu <- dim(nijtu)[4]
  NameD <- deparse(substitute(nijtu))
  stat <- Mod4.stat(nijtu)
  RVal <- list(statistic = c(D =stat),
        p.value = Mod4.pvalue(stat, nr, nc, nt, nu, sum(nijtu), B = 1e4),
        method = "Modular test for independence in four-way contingency table", data.name = NameD)
  class(RVal) <- "htest"
  return(RVal)
}
