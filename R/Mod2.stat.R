#' @title Modular test for independence in two-way contingency table
#'
#' @description
#' Calculates the statistic of the modular test for independence in two-way contingency table
#' (see Sulewski P. (2016)).
#'
#' @param nij a numeric matrix with non-negative values of the two-way contingency table cells
#' @return The function returns the value of the modular test statistic.
#' @rdname Mod2.stat
#'
#' @details
#' The statistic of the modular test for independence in r x c contingency table,
#' see formula (2) in the article.
#'
#' @author
#' Piotr Sulewski, \email{piotr.sulewski@apsl.edu.pl}, Pomeranian University in Slupsk.
#'
#' @references
#' {Sulewski, P. (2016). \emph{Moc testów niezależności w tablicy dwudzielczej większej niż 2×2,} Przegląd statystyczny 63(2), 190-210}
#'
#' @examples
#' tab5=GenTab2(matrix(1/12, nrow = 3, ncol = 4), 60)
#' Mod2.stat(tab5)
#' Mod2.stat(table1)
#'
#' @export

Mod2.stat <- function(nij) {
  nr <- nrow(nij); nc <- ncol(nij)
  nik <- rowSums(nij)
  nkj <- colSums(nij)
  E <- outer(nik, nkj, "*") / sum(nij)
  zero <- FALSE
  for (i in 1:nr) for (j in 1:nc) if (E[i,j] == 0) zero <-  TRUE
  if (zero == TRUE) stat = "Expected values must be nonzero" else stat=sum(abs(nij - E) / E)
  return (stat)
}
