#' M-step Estimate of Mean and Covariance
#'
#' Computes the M-step in the EM algorithm by estimating the mean vector and
#' covariance matrix from a fully observed (imputed) dataset.
#'
#' @param imputed_data A numeric matrix with no missing values.
#'
#' @return A list with elements:
#' \describe{
#'   \item{mu}{A numeric vector of column means.}
#'   \item{Sigma}{A numeric covariance matrix.}
#' }
#'
#' @details This function is typically called after the E-step has filled in
#' missing values, and is used to update the parameters of the multivariate normal model.
#'
#' @seealso \code{\link{run_em_algorithm}}, \code{\link{e_step_general_impute}}, \code{\link{initialize_parameters}}
#'
#' @export
#'
#' @examples
#' data <- matrix(c(1, 2, 3, 4, 5, 6), ncol = 2)
#' m_step_estimate(data)
m_step_estimate <- function(imputed_data) {
  .Call(`_DATA501AGM2_m_step_estimate`, imputed_data)
}