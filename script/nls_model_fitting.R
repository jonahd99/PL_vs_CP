eps <- 1e-6

library(tidyverse)

###############################################
# Sample from a truncated normal distribution #
###############################################

sample_from_truncated_norm <- function(n, lower_bound, upper_bound , mu, s = 1) {
  
  F_lb <- pnorm(lower_bound, mean = mu, sd = s)
  F_ub <- pnorm(upper_bound, mean = mu, sd = s)
  
  u <- runif(n, min = F_lb, max = F_ub)
  
  qnorm(u, mean = mu, sd = s)
  
}


sample_initial_values_running <- function(model, data_frame){
  
  cp_lr <- lm(data = data_frame, power ~ I(1/duration))
  
  init_W_aux <- cp_lr$coefficients["I(1/duration)"]
  init_CP_aux <- cp_lr$coefficients["(Intercept)"]
  
  if(model == "Omni" | model == "Om3CP" | model == "OmExp"){
    startPmax <- sample_from_truncated_norm(n = 1, lower_bound = 0,upper_bound = Inf, mu = 6, s = 3) 
    startA <- sample_from_truncated_norm(n = 1, lower_bound = 0,upper_bound = Inf, mu = 20, s = 20) 
    startW <- sample_from_truncated_norm(n = 1, lower_bound = 0,upper_bound = Inf, mu = init_W_aux, s = 10) 
    startCP <- sample_from_truncated_norm(n = 1, lower_bound = 0,upper_bound = startPmax, mu = init_CP_aux, s = 2) 
    startTau <- (startPmax - startCP)
    
    return(list(Tau = startTau,
                  A = startA,
                  W = startW,
                  CP = startCP))
  }else if(model == "PéT"){
    
    startE <- sample_from_truncated_norm(n = 1, lower_bound = -Inf ,upper_bound = 0, mu = -4, s = 4) 
    startA <- sample_from_truncated_norm(n = 1, lower_bound = 0,upper_bound = Inf, mu = 40, s = 30) 
    startMAP <- sample_from_truncated_norm(n = 1, lower_bound = 0,upper_bound = Inf, mu = init_CP_aux, s = 1)
    
    return(list(E = startE,
                  A = startA,
                  MAP = startMAP))
    
  }
  else if(model == "LJ"){
    
    startE <- sample_from_truncated_norm(n = 1, lower_bound = 0 ,upper_bound = 1, mu = 0.5, s = 0.3) 
    startW <- sample_from_truncated_norm(n = 1, lower_bound = 0,upper_bound = Inf, mu = init_W_aux, s = 30) 
    startCP <- sample_from_truncated_norm(n = 1, lower_bound = 0,upper_bound = Inf, mu = init_CP_aux, s = 1)
    
    return(list(E = startE,
                  W = startW,
                  CP = startCP))
  }else if(model == "3CP"){
    
    startB <- sample_from_truncated_norm(n = 1, lower_bound = 0,upper_bound = Inf, mu = 0.01, s = 0.1) 
    startAWC <- sample_from_truncated_norm(n = 1, lower_bound = 0,upper_bound = Inf, mu = init_W_aux, s = 30) 
    startCP <- sample_from_truncated_norm(n = 1, lower_bound = 0,upper_bound = Inf, mu = init_CP_aux, s = 1)
    
    return(list(B = startB,
                  AWC = startAWC,
                  CP = startCP))
  }else{return(NULL)}
}

sample_initial_values_rowing <- function(model, data_frame){
  
  cp_lr <- lm(data = data_frame, power ~ I(1/duration))
  
  init_W_aux <- cp_lr$coefficients["I(1/duration)"]
  init_CP_aux <- cp_lr$coefficients["(Intercept)"]
  
  if(model == "Omni" | model == "Om3CP" | model == "OmExp"){
    startPmax <- sample_from_truncated_norm(n = 1, lower_bound = 0,upper_bound = Inf, mu = 1500, s = 150) 
    startA <- sample_from_truncated_norm(n = 1, lower_bound = 0,upper_bound = Inf, mu = 20, s = 20) 
    startW <- sample_from_truncated_norm(n = 1, lower_bound = 0,upper_bound = Inf, mu = init_W_aux, s = 10) 
    startCP <- sample_from_truncated_norm(n = 1, lower_bound = 0,upper_bound = startPmax, mu = init_CP_aux, s = 2) 
    startTau <- (startPmax - startCP)
    
    return(list(Tau = startTau,
                A = startA,
                W = startW,
                CP = startCP))
  }else if(model == "PéT"){
    
    startE <- sample_from_truncated_norm(n = 1, lower_bound = -Inf ,upper_bound = 0, mu = -4, s = 4) 
    startA <- sample_from_truncated_norm(n = 1, lower_bound = 0,upper_bound = Inf, mu = 40, s = 30) 
    startMAP <- sample_from_truncated_norm(n = 1, lower_bound = 0,upper_bound = Inf, mu = init_CP_aux, s = 1)
    
    return(list(E = startE,
                A = startA,
                MAP = startMAP))
    
  }
  else if(model == "LJ"){
    
    startE <- sample_from_truncated_norm(n = 1, lower_bound = 0 ,upper_bound = 1, mu = 0.5, s = 0.3) 
    startW <- sample_from_truncated_norm(n = 1, lower_bound = 0,upper_bound = Inf, mu = init_W_aux, s = 30) 
    startCP <- sample_from_truncated_norm(n = 1, lower_bound = 0,upper_bound = Inf, mu = init_CP_aux, s = 1)
    
    return(list(E = startE,
                W = startW,
                CP = startCP))
  }else if(model == "3CP"){
    
    startB <- sample_from_truncated_norm(n = 1, lower_bound = 0,upper_bound = Inf, mu = 0.01, s = 0.1) 
    startAWC <- sample_from_truncated_norm(n = 1, lower_bound = 0,upper_bound = Inf, mu = init_W_aux, s = 30) 
    startCP <- sample_from_truncated_norm(n = 1, lower_bound = 0,upper_bound = Inf, mu = init_CP_aux, s = 1)
    
    return(list(B = startB,
                AWC = startAWC,
                CP = startCP))
  }else{return(NULL)}
}

###########################
#  Fit all NLS models     #
###########################

fit_nls_model <- function(model,data_frame,sport){
  
  if(model == "Omni" | model == "Om3CP" | model == "OmExp"){
    
    T_star_omni <- 1800
    
    T_star_om3cp <- 1800
    
    T_star_omExp <- 1800
    
    if(model == "Omni"){
      
      form <- as.formula( y~(W/x)*(1-exp(-x*((Tau)/W))) + CP - A*(log(x/T_star_omni))*(x>T_star_omni))
      
    }else if(model == "Om3CP") {
      
      form <- as.formula( y~W/(x +((W)/(Tau))) + CP - A*(log(x/T_star_om3cp))*(x>T_star_om3cp) )
      
    }else if(model == "OmExp") {
      
      form <- as.formula(y~(Tau)*exp(-(x)/(W*(exp(1)/(Tau)))) + CP - A*(log(x/T_star_omExp))*(x>T_star_omExp))
      
    }
    
    if(sport == "running"){
      
      init_list <- sample_initial_values_running(model = "Omni", data_frame)
      
    }else if(sport == "rowing"){
      
      init_list <- sample_initial_values_rowing(model = "Omni", data_frame)
    
    }
    
    mod <- nls(form, data=data_frame, 
               start=init_list, 
               lower = c(Tau = 0, A = 0,W = 0,CP = 0),
               algorithm = "port",
               trace = FALSE,
               control = list(warnOnly=FALSE))
  }else if( model == "PéT"){
    
    k_1 <- 30 # constant
    k_2 <- 20 # constant
    f <- -0.233 # constant
    BMR <- 1.2 # constant
    T_map <- 420 # constant
    
    form <- as.formula(y~((A*(1-exp(-x/k_2)) - k_1*(MAP - BMR)*(1-exp(-x/k_1)))/x) + MAP + (x>T_map) * log(x/T_map) * (((A*f*(1-exp(-x/k_2)) - k_1*E*(1-exp(-x/k_1)))/x) + E))
    
    if(sport == "running"){
      
      init_list <- sample_initial_values_running(model = "PéT", data_frame)
      
    }else if(sport == "rowing"){
      
      init_list <- sample_initial_values_rowing(model = "PéT", data_frame)
      
    }
    
    mod <-  nls(form, data=data_frame, 
                start=init_list, 
                lower = c(E = -Inf, A = 0, MAP = 0),
                upper = c(E = 0, A = Inf, MAP = Inf),
                algorithm = "port",
                trace = FALSE,
                control = list(warnOnly=FALSE))
  }else if(model == "LJ"){
    
    T_star <- 360
    
    S <- function(W,CP,E,T_star){
      W/(T_star^E) + CP/(T_star^(E-1))
    }
    
    ## LJ model fit
    form <- as.formula(y~((W/x) + CP ) * (x <= T_star) + (S(W,CP,E,T_star) * x^(E-1)) * (x > T_star))
    
    if(sport == "running"){
      
      init_list <- sample_initial_values_running(model = "LJ", data_frame)
      
    }else if(sport == "rowing"){
      
      init_list <- sample_initial_values_rowing(model = "LJ", data_frame)
      
    }
    
    mod <-  nls(form, data=data_frame, 
                start=init_list, 
                lower = c( E = 0,W = 0, CP = 0),
                upper = c(E = 1,W = Inf, CP = Inf),
                algorithm = "port",
                trace = FALSE,
                control = list(warnOnly=FALSE))
    
    
  }else if(model == "3CP"){
    # form <- as.formula(y~AWC/(x +((AWC)/(Pmax - CP))) + CP)
    form <- as.formula(y ~ AWC/(x + 1/B) + CP)
    
    if(sport == "running"){
      
      init_list <- sample_initial_values_running(model = "3CP", data_frame)
      
    }else if(sport == "rowing"){
      
      init_list <- sample_initial_values_rowing(model = "3CP", data_frame)
      
    }
    
    mod <-  nls(form, data=data_frame, 
                start=init_list,
                lower = c(B = 0,AWC = 0, CP = 0),
                algorithm = "port",
                trace = FALSE,
                control = list(warnOnly=FALSE))
  }
  return(mod) 
}

possibly_fit_nls_model <- possibly(.f = fit_nls_model, otherwise = NULL)

fit_nls_multiple_times <- function(model,data_frame, J,sport){

  output <- NULL
  for (j in 1:J) {
    #print(j)
    output <- possibly_fit_nls_model(model,data_frame,sport)
    
    if(!is.null(output)){
      break
    }
    
  }

  return(output)
}

############################
#  NLS time prediction     #
############################

fit_nls <- function(mod,t){
  df_fit <- data.frame(x = t)
  predict(mod, newdata = df_fit)
}

possibly_fit_nls <- possibly(.f = fit_nls, otherwise = NA)


fit_nls_distance <- function(mod,t){
  fit_nls(mod,t) * t
}


numerical_root_aux_function <- function(mod,t,d){
  d - fit_nls_distance(mod,t)
}

uniroot_func <- function(func = numerical_root_aux_function,
                         model,
                         ts,
                         dis,
                         tolerance= 0.00001){
  uniroot(func,
          mod = model,
          d = dis,
          tol = tolerance,
          interval = c(ts - ts*0.50,ts + ts*0.50),
          check.conv = TRUE)$root
}


possibly_uniroot_func <- possibly(.f = uniroot_func, otherwise = NA)



secant <- function(func = numerical_root_aux_function,
                   mod,
                   t_0,
                   t_1,
                   d, 
                   tol = 1e-04, 
                   niter = 2000){
  for (i in 1:niter) {
    t_2 <- t_1 - func(mod,t_1,d)*(t_1-t_0)/(func(mod,t_1,d)-func(mod,t_0,d))
    if(t_2 < 0){
      #print(paste("t_2 < 0 is",t_2 < 0))
      stop("negative t_2")
    } 
    
    if(abs(func(mod,t_2,d)) < tol) {
      #print(paste("abs(func(mod,t_2,d)) < tol is",abs(func(mod,t_2,d)) < tol))
      return(t_2)
    }
    t_0 <- t_1
    t_1 <- t_2
  }
  stop("exceeded allowed number of iterations")
}

possibly_secant <- possibly(.f = secant, otherwise = NA)

possibly_predict_time_from_nls <- function(mod,t,d){
  
  output <- possibly_secant(func = numerical_root_aux_function,
                  mod,
                  t_0 = t,
                  t_1 = t+1,
                  d, 
                  tol = 1e-04, 
                  niter = 2000)
  
  if(is.na(output)){
    return(possibly_secant(func = numerical_root_aux_function,
                           mod,
                           t_0 = t,
                           t_1 = t-1,
                           d, 
                           tol = 1e-04, 
                           niter = 2000))
    
  }
  
  #print(output)
  return(output)
  
  
}



#######################################
# Split training and testing dataset  #
#######################################

split_training_testing_dataset <- function(data_frame,method,d, split = 0.7){
  if(method == "random"){
    set.seed(123)
    
    data_frame %>% 
      mutate(row_id = row_number()) %>%
      sample_frac(split) %>%
      mutate(training = 1) -> training
    
    training_IDs <- training %>%
      pull(row_id)
    
    testing <-  data_frame %>% 
      mutate(row_id = row_number()) %>%
      filter(!row_id %in% training_IDs) %>%
      mutate(training = 0)

  }else if(method == "by_distance_up"){
    data_frame %>%
      filter(distance <= d) %>%
      mutate(training = 1)-> training
    data_frame %>%
      filter(distance > d) %>%
      mutate(training = 0) -> testing
  }else if(method == "by_distance_down"){
    data_frame %>%
      filter(distance >= d) %>%
      mutate(training = 1)-> training
    data_frame %>%
      filter(distance < d) %>%
      mutate(training = 0) -> testing
  }
  return(rbind(training,testing))
}


fit_pow_lr_time <- function(data_pl,t,d){
  pl_lr <- lm(data = data_pl, I(log(power)) ~ I(log(duration)))
  a <- as.numeric(exp(coef(pl_lr)[1]))
  b <- as.numeric(coef(pl_lr)[2])+1
  pl_power <-  (d/a)^(1/b)# a/(t^b)
  return(pl_power)
}

possibly_fit_pow_lr_time <- possibly(.f = fit_pow_lr_time, otherwise = NA)

fit_pow_lr_pow <- function(data_pl,t){
  pl_lr <- lm(data = data_pl, I(log(power)) ~ I(log(duration)))
  a <- as.numeric(exp(coef(pl_lr)[1]))
  b <- as.numeric(coef(pl_lr)[2])+1
  pl_power <-  a*(t^(b-1))
  return(pl_power)
}

possibly_fit_pow_lr_pow <- possibly(.f = fit_pow_lr_pow, otherwise = NA)


fit_cp_lr_pow <- function(data_cp,t){
  cp_lr <- lm(data = data_cp, power ~ I(1/duration))
  w_prime <- cp_lr$coefficients["I(1/duration)"]
  cp <- cp_lr$coefficients["(Intercept)"]
  cp_power <- w_prime/t + cp
  
  return(cp_power)
}
