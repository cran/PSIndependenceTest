#' @title Modular test for independence in two-way contingency table
#'
#' @description
#' Calculates the test statistic and p-value of the modular test for independence in two-way contingency table
#'
#' @param nij a numeric matrix with non-negative values of the two-way contingency table cells
#' @param B an integer specifying the number of replicates used in the Monte Carlo test (optional)
#' @return The function returns values of the test statistic and p-value of the modular test.
#' @rdname Mod2.test
#'
#' @details
#' The test statistic and p-value of the modular test for independence in r x c contingency table,
#'
#' @author
#' Piotr Sulewski, \email{piotr.sulewski@apsl.edu.pl}, Pomeranian University in Slupsk.
#'
#' @references
#' {Sulewski, P. (2016). \emph{Moc testów niezależności w tablicy dwudzielczej większej niż 2×2,} Przegląd statystyczny 63(2), 190-210}
#'
#' @examples
#' pij=matrix(1/4, nrow = 2, ncol = 2)
#' \donttest{Mod2.test(GenTab2(pij, 50), B = 1e3)}
#' \donttest{Mod2.test(table1, B = 1e2)}
#'
#' @export

Mod2.test <- function(nij, B = 1e4) {
  nr <- nrow(nij)
  nc <- ncol(nij)
  NameD <- deparse(substitute(nij))
  stat <- Mod2.stat(nij)
  RVal <- list(statistic = c(D = stat),
               p.value = Mod2.pvalue(stat, nr, nc, sum(nij), B = 1e4),
               method = "Modular test for independence in two-way contingency table",
               data.name = NameD)
  class(RVal) <- "htest"
  return(RVal)
}



