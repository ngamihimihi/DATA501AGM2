#' Run EM Algorithm for Imputation (Updating S3 Class)
#'
#' @param model An object of class 'em_model'
#' @param tolerance Convergence tolerance for log-likelihood
#' @param max_iter Maximum number of iterations
#'
#' @return Updated 'em_model' object
#' @export
run_em_algorithm <- function(model, tolerance = 1e-5, max_iter = 100) {
  stopifnot(inherits(model, "em_model"))
  
  data <- model$data
  params <- initialize_parameters(data)
  mu <- params$mu
  Sigma <- params$Sigma
  
  loglik_history <- c()
  parameter_history <- list(mu = list(), sigma = list())
  
  iter <- 0
  converged <- FALSE
  
  while (iter < max_iter) {
    iter <- iter + 1
    
    # --- E-step ---
    imputed_data <- DATA501AGM2::e_step_general_impute(data, mu, Sigma)
    
    # --- Log-likelihood ---
    loglik <- DATA501AGM2::log_likelihood_mvnorm(imputed_data, mu, Sigma)
    loglik_history <- c(loglik_history, loglik)
    
    # --- Save parameter history ---
    parameter_history$mu[[iter]] <- mu
    parameter_history$sigma[[iter]] <- Sigma
    
    # --- Convergence check ---
    if (iter > 1 && abs(loglik - loglik_history[iter - 1]) < tolerance) {
      converged <- TRUE
      break
    }
    
    # --- M-step ---
    estimates <- DATA501AGM2::m_step_estimate(imputed_data)
    mu <- estimates$mu
    Sigma <- estimates$Sigma
  }
  
  # Update and return model object
  model$loglik_history <- loglik_history
  model$parameters <- list(mu = mu, sigma = Sigma)
  model$parameter_history <- parameter_history
  model$imputed <- imputed_data
  model$early_stop$converged <- converged
  model$early_stop$iterations <- iter
  
  return(model)
}