#' @title Four-way contingency table r x c x t x u   - generation
#'
#' @description
#' Generating a four-way contingency table r x c x t x u.
#'
#' @param pijtu a numeric matrix with non-negative probability values of the four-way contingency table
#' @param n a sample size
#' @return The function returns the four-way contingency table r x c x t x u
#' @rdname GenTab4
#'
#' @details
#' Generating a four-way contingency table r x c x t x u using the probability matrix pijtu.
#' If Ho is true then pijtu equals 1 / r / c / t / u.
#'
#' @author
#' Piotr Sulewski, \email{piotr.sulewski@apsl.edu.pl}, Pomeranian University in Slupsk.
#'
#' @references
#' Extension of the information contained in {Sulewski, P. (2018). \emph{Power Analysis Of Independence Testing for the Three-Way Con-tingency Tables of Small Sizes.} Journal of Applied Statistics 45(13), 2481-2498}
#'
#' @examples
#' r = 2; c = 2; t = 2; u = 3
#' GenTab4(array(1 / (r * c * t * u), dim = c(r, c, t, u)),150)
#' table = GenTab4(array(1/16, dim = c(2, 2, 2, 2)), 200)
#' GenTab4(prop.table(table),200)
#'
#' @export


GenTab4 <- function(pijtu, n) {
  nr <- dim(pijtu)[1]
  nc <- dim(pijtu)[2]
  nt <- dim(pijtu)[3]
  nu <- dim(pijtu)[4]
  SumProb <- numeric(nr * nc * nt * nu)
  nijtu <- array(0, dim = c(nr, nc, nt, nu))
  k <- 1
  SumProb[1] <- 0
  for(i in 1:nr) {
    for(j in 1:nc) {
      for(t in 1:nt) {
        for(u in 1:nu) {
          k <- k + 1
          SumProb[k] <- SumProb[k - 1] + pijtu[i, j, t, u]
        }
      }
    }
  }
  for (d in 1:n) {
    los = stats::runif(1,0,1)
    k <- 1
    for(i in 1:nr) {
      for(j in 1:nc) {
        for(t in 1:nt) {
          for(u in 1:nu) {
            k <- k + 1
            if(los > SumProb[k - 1] & los <= SumProb[k]) nijtu[i, j, t, u] <- nijtu[i, j, t, u] + 1
          }
        }
      }
    }
  }
  return (nijtu)
}

