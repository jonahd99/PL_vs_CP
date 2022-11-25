###############################################################################
# Power law fitting function
###############################################################################

fit_pow_lr_pow <- function(data_pl,t){
  pl_lr <- lm(data = data_pl, I(log(power)) ~ I(log(durations)))
  a <- as.numeric(exp(coef(pl_lr)[1]))
  b <- as.numeric(-coef(pl_lr)[2])
  pl_power <- a/(t^b)
  return(pl_power)
}

fit_pow_lr_dis <- function(data_pl,t){
  pl_lr <- lm(data = data_pl, I(log(power)) ~ I(log(durations)))
  a <- as.numeric(exp(coef(pl_lr)[1]))
  b <- as.numeric(-coef(pl_lr)[2])
  pl_dis <- (a/(t^b)) * t
  return(pl_dis)
}


###############################################################################
# CP Fitting functions
###############################################################################

fit_cp_lr_pow <- function(data_cp,t){
  cp_lr <- lm(data = data_cp, power ~ I(1/durations))
  w_prime <- cp_lr$coefficients["I(1/durations)"]
  cp <- cp_lr$coefficients["(Intercept)"]
  cp_power <- w_prime/t + cp
  
  return(cp_power)
}

fit_cp_lr_dis <- function(data_cp,t){
  cp_lr <- lm(data = data_cp,  work ~ durations)
  w_prime <- cp_lr$coefficients["(Intercept)"]
  cp <- cp_lr$coefficients["durations"]
  cp_dis <-cp*t + w_prime 
  
  return(cp_dis)
}


######################
#  NLS functions PL  #
######################

nls_fit_pl <- function(pl_data,t){
  n_powers  <- length(pl_data$power)
  
  ols_pow <- lm(formula = I(log(pl_data$power)) ~ I(log(pl_data$durations)))
  # 
  # # Residual standard error on the backtransformed scale:
  sigma_ols_pow_pd <- sd(pl_data$power - exp(fitted(ols_pow))) * sqrt((n_powers - 1) / (n_powers - 2))
  # 
  # speed_init          <- max(exp(coef(ols_pow)[1]), eps)
  # fatigue_factor_init <- max(1 / (1 + coef(ols_pow)[2]) - 1, eps) + 1
  
  speed_init          <- 8
  fatigue_factor_init <- 1.1
  
  nls_pow <- nls(
    # formula = I(log(power)) ~ I(fatigue_factor * log(speed) + (1 - fatigue_factor) * log(work)), # power--work relationship (in log-space)
    formula = I(log(power)) ~ I(fatigue_factor * log(speed) + (1 - fatigue_factor) * log(work)), # power--work relationship (in log-space)
    data = pl_data,
    start = list(speed = speed_init, fatigue_factor = fatigue_factor_init),
    lower = c(0, 1),
    algorithm = "port",
    trace = FALSE,
    control = list(warnOnly = TRUE) # very rarely, the algorithm otherwise produce an error in small data sets which are such that the estimated value of W' is near zero
  )
  
  a <- as.numeric(nls_pow$m$getAllPars()["speed"])
  b <- as.numeric(nls_pow$m$getAllPars()["fatigue_factor"] )  
  
  return(a*t^((1/b)-1))
}


##############################
#  NLS functions Hyperbolic  #
##############################

nls_fit_hyp <- function(cp_data,t){
  # OLS estimate (used for initialising the optimiser):
  ols_hyp <- lm(formula = cp_data$power ~ I(1 / cp_data$durations))
  
  # Residual standard error:
  # sigma_hyp <- summary(ols_hyp)$sigma # note that this divides by n_powers - 2
  # sigma_hyp <- sd(powers - fitted(ols_hyp)) * sqrt((n_powers - 1) / (n_powers - 2)) # just to check that this gives the same result
  sigma_ols_hyp_pd <- mean(abs(1 - fitted(ols_hyp) / cp_data$power)) # relative errors
  
  w_prime_max <- min(cp_data$work) * 0.999 # W' must be less than the smallest amount of observed work/distance
  w_prime_min <- min(100 * eps, w_prime_max) # W' must also be non-negative
  w_prime_init <- coef(ols_hyp)[2] 
  
  w_prime_init <- ifelse(w_prime_init <= w_prime_max && 
                           w_prime_init >= w_prime_min, w_prime_init, 
                         w_prime_min + (w_prime_max - w_prime_min) / 2)
  
  cp_init <- max(coef(ols_hyp)[1], eps) # CP must be non-negative
  
  # Estimation by nonlinear weighted least squares using the power--work 
  # (i.e. velocity--distance) relationship with weights proportional to
  # 1 / work as recommended in Patoz et al, 
  # "Critical speed estimated by statistically appropriate fitting 
  # procedures", EJAP, 121:2027–-2038, 2021.
  nls_hyp <- nls(
    # formula = power ~ I(w_prime / duration + cp), # power--work relationship
    formula = power ~ I(cp / (1 - w_prime / work)), # power--work relationship
    data = cp_data,
    # weights = 1 / duration,
    weights = 1 / work,
    start = list(w_prime = w_prime_init, cp = cp_init),
    lower = c(0, 0),
    upper = c(w_prime_max, Inf),
    algorithm = "port",
    trace = FALSE,
    control = list(warnOnly = TRUE) # very rarely, the algorithm otherwise produce an error in small data sets which are such that the estimated value of W' is near zero
  )
  
  w_prime <- nls_hyp$m$getAllPars()["w_prime"]
  cp <- nls_hyp$m$getAllPars()["cp"]
  print(w_prime)
  print(cp)
  return(w_prime/t + cp)
  
}


##############################
#  NLS functions Omni        #
##############################


fit_Omni <- function(data_frame,startCP,StartW){
  t <- 1:7200
  
  ##set your start values for parameters
  startPmax <- 1000
  startA <- 30
  startT_star <- 1000
  
  ##Omni model fit
  form <- as.formula( y~(W1/x)*(1-exp(-x*((Pmax - CP)/W1))) + CP - A*(log(x/T_star))*(x>T_star))
  
  mod <- nls(form, data=data_frame, start=list(W1=startW, Pmax=startPmax, CP=startCP, A=startA, T_star = startT_star), lower = rep(1, times = 5), algorithm = "port")
  W1 <- as.numeric(mod$m$getAllPars()[1])
  #Pmax <- as.numeric(mod$m$getAllPars()[2])
  CP <- as.numeric(mod$m$getAllPars()[3])
  A <- as.numeric(mod$m$getAllPars()[4])
  
  df_fit <- data.frame(x = 1:7200)
  omni_pd_leo_fit <- predict(mod, newdata = df_fit)
  #print(A)
  return(omni_pd_leo_fit)
  
}

##############################
#  NLS functions 3cp        #
##############################

fit_Omni3cp <- function(data_frame,startCP,StartW){
  ##set your start values for parameters
  startPmax <- 1000
  startA <- 30
  startT_star <- 1000
  
  ##Omni3cp model fit
  form <- as.formula( y~W1/(x +((W1)/(Pmax - CP))) + CP - A*(log(x/T_star))*(x>T_star) )
  mod <- nls(form, data=data_frame, start=list(W1=startW, Pmax=startPmax, CP=startCP, A=startA, T_star = startT_star), lower = rep(1, times = 5), algorithm = "port")
  W1 <- as.numeric(mod$m$getAllPars()[1])
  Pmax <- as.numeric(mod$m$getAllPars()[2])
  CP <- as.numeric(mod$m$getAllPars()[3])
  A <- as.numeric(mod$m$getAllPars()[4])
  
  df_fit <- data.frame(x = 1:7200)
  omni_3cp_pd_leo_fit <- predict(mod, newdata = df_fit)
  
  return(omni_3cp_pd_leo_fit)
  
}

##############################
#  NLS functions OmExp        #
##############################
fit_OmExp <- function(data_frame,startCP,StartW){
  t <- 1:7200
  
  ##set your start values for parameters
  startPmax <- 1000
  startA <- 30
  startT_star <- 1000
  
  ##ExPD model fit
  form <- as.formula(y~(Pmax - CP)*exp(-(x)/(W1*(exp(1)/(Pmax - CP)))) + CP - A*(log(x/T_star))*(x>T_star))
  
  mod <- nls(form, data=data_frame, start=list(W1=startW, Pmax=startPmax, CP=startCP, A=startA, T_star = startT_star), lower = rep(1, times = 5), algorithm = "port")
  W1 <- as.numeric(mod$m$getAllPars()[1])
  Pmax <- as.numeric(mod$m$getAllPars()[2])
  CP <- as.numeric(mod$m$getAllPars()[3])
  A <- as.numeric(mod$m$getAllPars()[4])
  
  df_fit <- data.frame(x = 1:7200)
  omni_exp_pd_leo_fit <- predict(mod, newdata = df_fit)
  
  return(omni_exp_pd_leo_fit)
  
}
