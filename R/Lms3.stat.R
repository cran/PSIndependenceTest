#' @title Logarithmic minimum test for independence in three-way contingency table
#'
#' @description
#' Calculates the statistic of the Logarithmic minimum test for independence in three-way contingency table
#' (see Sulewski P. (2018)).
#'
#' @param nijt a numeric matrix with non-negative values of the three-way contingency table cells
#' @return The function returns the value of the logarithmic minimum test statistic.
#' @rdname Lms3.stat
#'
#' @details
#' The statistic of Logarithmic minimum test for independence in r x c x t contingency table,
#' see formula (6) in the article.
#'
#' @author
#' Piotr Sulewski, \email{piotr.sulewski@apsl.edu.pl}, Pomeranian University in Slupsk.
#'
#' @references
#' {Sulewski, P. (2021). \emph{Logarithmic Minimum Test for Independence in Three Way Con-tingency Table of Small Sizes,} Journal of Statistical Computation and Simulation 91(13), 2780-2799}
#'
#' @examples
#' Lms3.stat(table3)
#' Lms3.stat(GenTab3(array(1/12, dim=c(2,2,3)), 120))
#'
#' @export

Lms3.stat <- function(nijt) {
  nr <- dim(nijt)[1]
  nc <- dim(nijt)[2]
  nt <- dim(nijt)[3]
  nikk <- numeric(nr)
  nkjk <- numeric(nc)
  nkkt <- numeric(nt)
  n <- sum(nijt)
  E <- array(NA, dim = c(nr, nc, nt))
  S <- 0
  for(t in 1:nt) for(j in 1:nc) for(i in 1:nr) nikk[i] <- nikk[i] + nijt[i,j,t]
  for(t in 1:nt) for(i in 1:nr) for(j in 1:nc) nkjk[j] <- nkjk[j] + nijt[i,j,t]
  for(i in 1:nr) for(j in 1:nc) for(t in 1:nt) nkkt[t] <- nkkt[t] + nijt[i,j,t]
  for(t in 1:nt) for(j in 1:nc) for(i in 1:nr) E[i,j,t] <- nikk[i] * nkjk[j] * nkkt[t] / n / n

  zero <- FALSE
  for(t in 1:nt) for(j in 1:nc) for(i in 1:nr) if (E[i,j,t] == 0) zero <-  TRUE
  if (zero == TRUE) stat = "Expected values must be nonzero" else {
    S <- 0
    for(t in 1:nt) for(j in 1:nc) for(i in 1:nr)  S <- S + log(min(nijt[i, j, t], E[i, j, t]) /
                                                  max(nijt[i, j, t], E[i, j, t]) + 1e-5)
    stat <- -S
  }
  return (stat)
}
