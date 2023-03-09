
# ------------------------------------------------------------------------------
# About this code
# Run Kalman filtering: logistic regression augmented with latent variable
# see Bakerman et al.(2022)
# ------------------------------------------------------------------------------

FFSB_logit <- function(X,           # tibble of covariates   (+ year and country)
                       Y,           # tibble of crisis dummy (+ year and country)
                       W = NULL     # variance of the state equation
                       ){
  
  # create a vector of year
  year <- 
    Y %>% 
    select(year) %>% 
    unique() %>% 
    unlist()
  
  
  # initialize parameters
  n     <- length(year)  # max length of time
  p     <- dim(X)[2] - 2 # number of covariates
  omega <- rpg(n, 1, 0)  # simulated omega
  G     <- diag(p)       # transition matrix of the beta: random walk

  # if W is not indicated, save as an identity matrix
  if (is.null(W)) {
    W <- diag(p) / 100
  }
  
  # initial guess for the states
  theta.prior <- rep(0, p) # posterior mean of the betas in current period
  cov.prior   <- diag(p) / 100   # posterior covariance matrix of the betas in current period

  # list to save the Kalman filtered results
  theta.list <- vector("list", length = n) # posterior mean of states
  cov.list   <- vector("list", length = n) # posterior var/cov matrix of states

# ------------------------------------------------------------------------------
# Forward Filtering
# ------------------------------------------------------------------------------

  for(i in 1:n){
    
    # check the current year
    t <- year[i]
    
    # Set the current economic variables (p * n.country matrix of variables in period i)
    Ft <- 
      X %>% 
      filter(year == t) %>% 
      select(-year, -country) %>% 
      as.matrix()
    
    ctry.t <- 
      X %>% 
      filter(year == t) %>% 
      select(country) 
    
    zt <- 
      Y %>% 
      filter(year == t) %>% 
      mutate(z = (as.numeric(crisis) - 1 - 1/2) / omega[i]) %>% 
      select(z) %>% 
      as.matrix()
    
    V <- diag(1/omega[i], nrow(ctry.t))
      
    # One step ahead predictive distribution of states (\beta_t|y_{1:t-1})
    theta.pred <- G %*% theta.prior # one ahead mean of states (beta)
    cov.pred   <- G %*% cov.prior %*% t(G) + W # one ahead variance of states
    
    # check the forecast error
    error.pred <- zt - Ft %*% G %*% theta.prior
    
    # posterior mean of theta
    theta.post <- 
      theta.pred + cov.pred %*% t(Ft) %*% solve(V + Ft %*% cov.pred %*% t(Ft)) %*% error.pred
    
    # posterior covariance matrix of theta
    cov.post <- 
      cov.pred - cov.pred %*% t(Ft) %*% solve(V + Ft %*% cov.pred %*% t(Ft)) %*% Ft %*% cov.pred
    
    # Collect posteriors
    theta.list[[i]] <- theta.post
    cov.list[[i]]   <- cov.post
    
    # update posteriors as the priors of the next period
    theta.prior <- theta.post
    cov.prior   <- cov.post
  }
  
  return(list(theta.list, cov.list))
}
