#' @title Modular test for independence in three-way contingency table
#'
#' @description
#' Calculates the statistic of the modular test for independence in three-way contingency table
#' (see Sulewski P. (2018)).
#'
#' @param nijt a numeric matrix with non-negative values of the three-way contingency table cells
#' @return The function returns the value of the modular test statistic.
#' @rdname Mod3.stat
#'
#' @details
#' The statistic of the modular test for independence in r x c x t contingency table,
#' see formula (6) in the article.
#'
#' @author
#' Piotr Sulewski, \email{piotr.sulewski@apsl.edu.pl}, Pomeranian University in Slupsk.
#'
#' @references
#' {Sulewski, P. (2018). \emph{Power Analysis Of Independence Testing for the Three-Way Con-tingency Tables of Small Sizes.} Journal of Applied Statistics 45(13), 2481-2498}
#'
#' @examples
#' Mod3.stat(GenTab3(array(0.125, dim = c(2, 2, 2)), 100))
#' Mod3.stat(table4)
#'
#' @export

Mod3.stat <- function(nijt) {
  nr <- dim(nijt)[1]
  nc <- dim(nijt)[2]
  nt <- dim(nijt)[3]
  nikk <- numeric(nr)
  nkjk <- numeric(nc)
  nkkt <- numeric(nt)
  n <- sum(nijt)
  E <- array(NA, dim = c(nr, nc, nt))
  for(t in 1:nt) for(j in 1:nc) for(i in 1:nr) nikk[i] <- nikk[i] + nijt[i,j,t]
  for(t in 1:nt) for(i in 1:nr) for(j in 1:nc) nkjk[j] <- nkjk[j] + nijt[i,j,t]
  for(i in 1:nr) for(j in 1:nc) for(t in 1:nt) nkkt[t] <- nkkt[t] + nijt[i,j,t]
  for(t in 1:nt) for(j in 1:nc) for(i in 1:nr) E[i,j,t] <- nikk[i] * nkjk[j] * nkkt[t] / n / n
  zero <- FALSE
  for(t in 1:nt) for(j in 1:nc) for(i in 1:nr) if (E[i,j,t] == 0) zero=TRUE
  if (zero == TRUE) stat = "Expected values must be nonzero" else stat=sum(abs(nijt - E) / E)
  return (stat)
}


