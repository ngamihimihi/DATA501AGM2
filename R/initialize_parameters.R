#' Initialize Parameters from Incomplete Data
#'
#' Computes initial estimates for the mean vector (`mu`) and covariance matrix (`Sigma`)
#' from a numeric matrix containing missing values.
#'
#' @param data A numeric matrix with missing values (NAs). Must contain at least one non-missing value per column.
#'
#' @return A list with elements:
#' \describe{
#'   \item{mu}{A numeric vector of column-wise means (ignoring NAs).}
#'   \item{Sigma}{A covariance matrix estimated using pairwise complete observations.}
#' }
#'
#' @details This function provides a simple initialization for the EM algorithm,
#' assuming missing values are ignorable for first-pass estimates. It uses
#' \code{colMeans(..., na.rm = TRUE)} and \code{cov(..., use = "pairwise.complete.obs")}.
#'
#' @seealso \code{\link{run_em_algorithm}}, \code{\link{e_step_general_impute}}
#'
#' @export
#'
#' @examples
#' data <- matrix(c(1, NA, 3, 4, 5, 6), ncol = 2)
#' init <- initialize_parameters(data)
#' init$mu
#' init$Sigma
initialize_parameters <- function(data) {
  mu <- colMeans(data, na.rm = TRUE)
  Sigma <- cov(data, use = "pairwise.complete.obs")  # Can later switch to EM-based if needed
  return(list(mu = mu, Sigma = Sigma))
}