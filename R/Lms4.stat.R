#' @title Logarithmic minimum test for independence in four-way contingency table
#'
#' @description
#' Calculates the statistic of the Logarithmic minimum test for independence in four-way contingency table
#'
#' @param nijtu a numeric matrix with non-negative values of the four-way contingency table cells
#' @return The function returns the value of the logarithmic minimum test statistic.
#' @rdname Lms4.stat
#'
#' @details
#' The statistic of Logarithmic minimum test for independence in r x c x t x u contingency table,
#'
#' @author
#' Piotr Sulewski, \email{piotr.sulewski@apsl.edu.pl}, Pomeranian University in Slupsk.
#'
#' @references
#' Extension of the information contained in {Sulewski, P. (2021). \emph{Logarithmic Minimum Test for Independence in Three Way Con-tingency Table of Small Sizes,} Journal of Statistical Computation and Simulation 91(13), 2780-2799}
#'
#' @examples
#' Lms4.stat(GenTab4(array(1/16, dim = c(2, 2, 2, 2)), 160))
#' Lms4.stat(table5)
#'
#' @export

Lms4.stat <- function(nijtu) {
  nr <- dim(nijtu)[1]
  nc <- dim(nijtu)[2]
  nt <- dim(nijtu)[3]
  nu <- dim(nijtu)[4]
  nikkk <- numeric(nr)
  nkjkk <- numeric(nc)
  nkktk <- numeric(nt)
  nkkku <- numeric(nu)
  n <- sum(nijtu)
  E <- array(NA, dim = c(nr, nc, nt, nu))
  S <- 0
  for(u in 1:nu) for(t in 1:nt) for(j in 1:nc) for(i in 1:nr) nikkk[i] <- nikkk[i] + nijtu[i,j,t,u]
  for(u in 1:nu) for(t in 1:nt) for(i in 1:nr) for(j in 1:nc) nkjkk[j] <- nkjkk[j] + nijtu[i,j,t,u]
  for(u in 1:nu) for(i in 1:nr) for(j in 1:nc) for(t in 1:nt) nkktk[t] <- nkktk[t] + nijtu[i,j,t,u]
  for(i in 1:nr) for(j in 1:nc) for(t in 1:nt) for(u in 1:nu) nkkku[u] <- nkkku[u] + nijtu[i,j,t,u]
  for(u in 1:nu) for(t in 1:nt) for(j in 1:nc) for(i in 1:nr) E[i,j,t,u] <- nikkk[i] * nkjkk[j] * nkktk[t] * nkkku[u] / n / n / n

  zero <- FALSE
  for(u in 1:nu) for(t in 1:nt) for(j in 1:nc) for(i in 1:nr) if (E[i,j,t, u] == 0) zero <-  TRUE
  if (zero == TRUE) stat = "Expected values must be nonzero" else {
    S <- 0
    for(u in 1:nu) for(t in 1:nt) for(j in 1:nc) for(i in 1:nr)  S <- S + log(min(nijtu[i, j, t, u], E[i, j, t, u]) /
                                                                 max(nijtu[i, j, t, u], E[i, j, t, u]) + 1e-5)
    stat <- -S
  }
  return (stat)
}
