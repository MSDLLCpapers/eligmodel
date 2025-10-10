#' elig_data_gen: Generate example eligibility data
#'
#' Function that generates example eligibility data. Key outputs include Y (outcome of interest),
#' and X (baseline covariates; includes eligibility variables). Participants are considered eligible
#' if X1>0, X2>0, and X3>0.

#' @param n Number of observations (default=1000)
#' @param dim_X Number of baseline covariates (default=10)
#' @param rho Pairwise correlation of X variables (default=0.30)
#' @param miss_pct Missingness percentage (assumes MCAR, default=0.10)
#' @return
#' \itemize{
#' \item Y (continuous outcome of interest)
#' \item X (baseline covariates, with missing)
#' \item elig_dat (matrix of eligibility indicators; for X1-X3)
#' }
#' @export
#' @importFrom mvtnorm rmvnorm
#'
elig_data_gen <- function(n=1000, dim_X = 10, rho=0.30, miss_pct = 0.10) {

  # Generate Data #
  mu_vec <- rep(0, dim_X)
  jd <- function(n,m) matrix(c(1),nrow=n,ncol=m)
  Sigma_X <- diag(dim_X)+jd(dim_X,dim_X)*rho - diag(dim_X)*rho
  X <- data.frame(mvtnorm::rmvnorm(n=n, mean = mu_vec))
  X$X10 <- ifelse(X$X10 < -0.20, "C",
                  ifelse(X$X10 < 0.40, "B", "A"))
  Y <- 1*X[,1] + 1*X[,2] + 1*X[,3] + 0.5*X[,4] + 0.25*X[,5] + rnorm(n=n)

  # Randomly generate missing values #
  create_miss <- function(Xdat, miss_pct) {

    for (j in 1:dim(Xdat)[2]) {
      miss_ind <- rbinom(n=n, size=1, prob=miss_pct)
      Xdat[,j] <- ifelse(miss_ind==1, NA, Xdat[,j])
    }
    return(Xdat)
  }
  X_miss <- create_miss(Xdat=X, miss_pct=miss_pct)
  X_miss$X10 <- factor(X_miss$X10)

  # Create eligibility indicators #
  elig_dat <- data.frame(X1_IE = ifelse(X_miss$X1 > 0, 1, 0),
                         X2_IE = ifelse(X_miss$X2 > 0, 1, 0),
                         X3_IE = ifelse(X_miss$X3 > 0, 1, 0))

  output <- list(Y=Y, X=X_miss, elig_dat=elig_dat)

  return(output)
}
