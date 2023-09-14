#' @title Three-way contingency table r x c x t  - generation
#'
#' @description
#' Generating a three-way contingency table r x c x t.
#'
#' @param pijt a numeric matrix with non-negative probability values of the three-way contingency table
#' @param n a sample size
#' @return The function returns the three-way contingency table r x c x t
#' @rdname GenTab3
#'
#' @details
#' Generating a three-way contingency table r x c x t using the probability matrix pijt.
#' If Ho is true then pijt equals 1 / r / c / t.
#'
#' @author
#' Piotr Sulewski, \email{piotr.sulewski@apsl.edu.pl}, Pomeranian University in Slupsk.
#'
#' @references
#' {Sulewski, P. (2018). \emph{Power Analysis Of Independence Testing for the Three-Way Con-tingency Tables of Small Sizes.} Journal of Applied Statistics 45(13), 2481-2498}
#'
#' @examples
#' r = 2; c = 3; t = 4
#' GenTab3(array(1 / (r * c * t), dim = c(r, c, t)),250)
#' table = GenTab3(array(0.125, dim = c(2, 2, 2)), 100)
#' GenTab3(prop.table(table),100)
#'
#' @export


GenTab3 <- function(pijt, n) {
  nr <- dim(pijt)[1]
  nc <- dim(pijt)[2]
  nt <- dim(pijt)[3]
  SumProb <- numeric(nr * nc * nt)
  nijt <- array(0, dim = c(nr, nc, nt))
  u <- 1
  SumProb[1] <- 0
  for(i in 1:nr) {
    for(j in 1:nc) {
      for(t in 1:nt) {
        u <- u + 1
        SumProb[u] <- SumProb[u - 1] + pijt[i, j, t]
      }
    }
  }
  for (d in 1:n) {
    los <- stats::runif(1,0,1)
    u <- 1
    for(i in 1:nr) {
      for(j in 1:nc) {
        for(t in 1:nt) {
          u <- u + 1
          if(los > SumProb[u - 1] & los <= SumProb[u]) nijt[i, j, t] <- nijt[i, j, t] + 1
        }
      }
    }
  }
  return (nijt)
}

