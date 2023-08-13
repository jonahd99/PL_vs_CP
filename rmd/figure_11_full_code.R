################################################################################
## SETUP
################################################################################

library("here")
library("tidyverse")
library("R.utils")

source(here("script","nls_model_fitting.R"))


options(scipen=999)

################################################################################
## Data
################################################################################
split_data <- FALSE

models <- c("Omni","Om3CP","OmExp","LJ","PéT","3CP","Power law")

if(split_data == TRUE){running_data <-readRDS(here("figure_data","df_power_of_10.Rds")) %>%
  select(id,duration,distance,power,date) %>%
  mutate(work = distance,
         x = duration,
         y = power)

running_data %>%
  mutate(row_id = row_number()) %>%
  group_by(id) %>%
  filter(n_distinct(distance) >= 4 & n_distinct(row_id) >= 7) %>% 
  ungroup() %>%
  pull(id) %>%
  unique() -> ID

I <- length(ID)

running_data_test_train <- tibble()


for (i in 1:I) {
  print(paste(i,"/",I))
  
  athlete_i_data <- running_data %>%
    filter(id == ID[i])  
  
  L_i <- nrow(athlete_i_data)
  
  athlete_i_test_train_aux <- split_training_testing_dataset(data_frame = athlete_i_data,
                                                             method = "random",d = NULL, split = 6/L_i)
  
  running_data_test_train <- rbind(running_data_test_train,athlete_i_test_train_aux)
  
  saveRDS(running_data_test_train, here("figure_data","running_data_test_train.RDS"))
  
}}else{
  
  running_data_test_train <- readRDS(here("figure_data","running_data_test_train.RDS"))
  
}

re_calc_errors <- FALSE

if(re_calc_errors == TRUE){
  
  running_data <-readRDS(here("figure_data","df_power_of_10.Rds")) %>%
    select(id,duration,distance,power,date) %>%
    mutate(work = distance,
           x = duration,
           y = power)
  
  running_data %>%
    mutate(row_id = row_number()) %>%
    group_by(id) %>%
    filter(n_distinct(distance) >= 4 & n_distinct(row_id) >= 7) %>% 
    ungroup() %>%
    pull(id) %>%
    unique() -> ID
  
  I <- length(ID)
  
  no_params <- c(4,4,4,3,3,3,2)
  
  J_values <- c(150,150,150,50,50,50,NA)
  
  t_star <- c(1800,1800,1800,360,420,NA,NA)
  
  mods <- tibble(models,
                 no_params,
                 t_star,
                 J = J_values)
  
  error_calc_tib <- tibble()
  
  
  ################################################################################
  ## Iterating over each athlete
  ################################################################################
  
  for(i in 1:I){
    
    print(paste(i,"/",I))
    
    ##############################################################################
    ## Filter data for athlete i
    ##############################################################################
    
    athlete_i_data <- running_data %>%
      filter(id == ID[i])  
    
    L_i <- nrow(athlete_i_data)
    
    ##############################################################################
    ## Split testing and training dataset
    ##############################################################################
    
    athlete_i_test_train_aux <- split_training_testing_dataset(data_frame = athlete_i_data,
                                                               method = "random",d = NULL, split = 6/L_i)
    
    training <- athlete_i_test_train_aux %>%
      filter(training == 1)
    
    testing <- athlete_i_test_train_aux %>%
      filter(training == 0)
    
    ##############################################################################
    ## Check the number of data points
    ##############################################################################
    
    L <- nrow(training)
    
    K <- nrow (testing)
    
    if(L <= 2){
      error_tib_aux <- tibble(id = rep(ID[i], times = length(models)),
                              duration = rep(NA, times = length(models)),
                              distance = rep(NA, times = length(models)),
                              power = rep(NA, times = length(models)),
                              date = rep(NA, times = length(models)),
                              prediction = rep(NA, times = length(models)),
                              no_train_data = rep(L, times = length(models)),
                              no_test_data = rep(K, times = length(models)),
                              model = models,
                              error_message = rep("Not enough training data points", times = length(models)))
    }else if(K == 0){
      
      error_tib_aux <- tibble(id = rep(ID[i], times = length(models)),
                              duration = rep(NA, times = length(models)),
                              distance = rep(NA, times = length(models)),
                              power = rep(NA, times = length(models)),
                              date = rep(NA, times = length(models)),
                              prediction = rep(NA, times = length(models)),
                              no_train_data = rep(L, times = length(models)),
                              no_test_data = rep(K, times = length(models)),
                              model = models,
                              error_message = rep("Not enough testing data points", times = length(models)))
      
    }else{
      
      ############################################################################
      ## Iterating over each model
      ############################################################################
      error_calc_tib_aux <- tibble()
      
      for (m in models) {
        
        ##########################################################################
        ## Power-law model is fit using a different function to NLS models 
        ##########################################################################
        
        if(m == "Power law"){
          
          ########################################################################
          ## Fit power-law model and make predictions 
          ########################################################################
          
          pl_predictions <- possibly_fit_pow_lr_time(data_pl = training,t = testing$duration, d = testing$distance)
          
          ########################################################################
          ## Checking whether the prediction was made 
          ########################################################################
          
          if(NA %in% pl_predictions){
            model_error_tib_aux <- tibble(id = rep(ID[i], times = K),
                                          duration = rep(NA, times = K),
                                          distance = rep(NA, times = K),
                                          power = rep(NA, times = K),
                                          date = rep(NA, times = K),
                                          prediction = rep(NA, times = K),
                                          no_train_data = rep(L, times = K),
                                          no_test_data = rep(K, times = K),
                                          model = rep(m, times = K),
                                          error_message = rep("Unable to fit model", times = K))
          }else{
            model_error_tib_aux <- testing %>%
              mutate(prediction = pl_predictions,
                     no_train_data = L,
                     no_test_data = K,
                     model = m,
                     error_message = "No error") %>%
              select(id,duration,distance,power,date,prediction,no_train_data,no_test_data,model,error_message)
          }
          
        }else if(m == "3CP"){
          
          m_no_params <- mods %>%
            filter(models == m) %>%
            pull(no_params)
          
          J <- mods %>%
            filter(models == m) %>%
            pull(J)
          
          
          if(L <= m_no_params){
            
            model_error_tib_aux <- testing %>%
              mutate(prediction = NA,
                     no_train_data = L,
                     no_test_data = K,
                     model = m,
                     error_message = "Not enough training data points") %>%
              select(id,duration,distance,power,date,prediction,no_train_data,no_test_data,model,error_message)
            
          }else{
            
            model_m_mod <- fit_nls_multiple_times(model = m, data_frame = training, J, sport = "running")
            
            if(is.null(model_m_mod)){
              
              model_error_tib_aux <- testing %>%
                mutate(prediction = NA,
                       no_train_data = L,
                       no_test_data = K,
                       model = m,
                       error_message = "Unable to fit the model") %>%
                select(id,duration,distance,power,date,prediction,no_train_data,no_test_data,model,error_message)
              
            }else{
              
              ########################################################################
              ## Iterating over the testing dataset to make predictions
              ########################################################################
              model_error_tib_aux <- tibble()
              
              for(k in 1:K){
                
                
                model_m_prediction <- possibly_uniroot_func(model = model_m_mod,
                                                            ts = testing$duration[k],
                                                            dis = testing$distance[k])
                # print(model_m_prediction)
                if(is.na(model_m_prediction)){
                  
                  model_m_aux <- testing %>%
                    filter(row_id == testing$row_id[k]) %>%
                    mutate(prediction = model_m_prediction,
                           no_train_data = L,
                           no_test_data = K,
                           model = m,
                           error_message = "Unable to find root") %>%
                    select(id,duration,distance,power,date,prediction,no_train_data,no_test_data,model,error_message)
                  
                }else{
                  
                  model_m_aux <- testing %>%
                    filter(row_id == testing$row_id[k]) %>%
                    mutate(prediction = model_m_prediction,
                           no_train_data = L,
                           no_test_data = K,
                           model = m,
                           error_message = "No error") %>%
                    select(id,duration,distance,power,date,prediction,no_train_data,no_test_data,model,error_message)
                  
                  
                }
                model_error_tib_aux <- rbind(model_error_tib_aux,model_m_aux) # Do this bit for 3CP!
                
              }
              
            }
            
          }
          
        }
        else{
          ########################################################################
          ## Checking the number of data points again - this time for t_star
          ########################################################################
          
          m_T_star <- mods %>%
            filter(models == m) %>%
            pull(t_star)
          
          m_no_params <- mods %>%
            filter(models == m) %>%
            pull(no_params)
          
          J <- mods %>%
            filter(models == m) %>%
            pull(J)
          
          training %>%
            filter(duration < m_T_star) %>%
            nrow() -> m_no_below
          training %>%
            filter(duration > m_no_params) %>%
            nrow() -> m_no_above
          
          if(L <= m_no_params){
            
            model_error_tib_aux <- testing %>%
              mutate(prediction = NA,
                     no_train_data = L,
                     no_test_data = K,
                     model = m,
                     error_message = "Not enough training data points") %>%
              select(id,duration,distance,power,date,prediction,no_train_data,no_test_data,model,error_message)
          }else if(m_no_below < 2){
            
            model_error_tib_aux <- testing %>%
              mutate(prediction = NA,
                     no_train_data = L,
                     no_test_data = K,
                     model = m,
                     error_message = "Not enough training data below T_star") %>%
              select(id,duration,distance,power,date,prediction,no_train_data,no_test_data,model,error_message)
            
          }else if(m_no_above < 2){
            
            model_error_tib_aux <- testing %>%
              mutate(prediction = NA,
                     no_train_data = L,
                     no_test_data = K,
                     model = m,
                     error_message = "Not enough training data above T_star") %>%
              select(id,duration,distance,power,date,prediction,no_train_data,no_test_data,model,error_message)
            
          } else{
            
            model_m_mod <- fit_nls_multiple_times(model = m, data_frame = training, J, sport = "running")
            
            if(is.null(model_m_mod)){
              
              model_error_tib_aux <- testing %>%
                mutate(prediction = NA,
                       no_train_data = L,
                       no_test_data = K,
                       model = m,
                       error_message = "Unable to fit the model") %>%
                select(id,duration,distance,power,date,prediction,no_train_data,no_test_data,model,error_message)
              print(paste("Unable to fit the,",m,"model"))
              
            }else{
              
              ########################################################################
              ## Iterating over the testing dataset to make predictions
              ########################################################################
              model_error_tib_aux <- tibble()
              
              for(k in 1:K){
                
                model_m_prediction <- possibly_uniroot_func(model = model_m_mod,
                                                            ts = testing$duration[k],
                                                            dis = testing$distance[k])
                # print(model_m_prediction)
                if(is.na(model_m_prediction)){
                  
                  model_m_aux <- testing %>%
                    filter(row_id == testing$row_id[k]) %>%
                    mutate(prediction = model_m_prediction,
                           no_train_data = L,
                           no_test_data = K,
                           model = m,
                           error_message = "Unable to find root") %>%
                    select(id,duration,distance,power,date,prediction,no_train_data,no_test_data,model,error_message)
                  
                }else{
                  
                  model_m_aux <- testing %>%
                    filter(row_id == testing$row_id[k]) %>%
                    mutate(prediction = model_m_prediction,
                           no_train_data = L,
                           no_test_data = K,
                           model = m,
                           error_message = "No error") %>%
                    select(id,duration,distance,power,date,prediction,no_train_data,no_test_data,model,error_message)
                  
                  
                }
                model_error_tib_aux <- rbind(model_error_tib_aux,model_m_aux) # Do this bit for 3CP!
                
              }
              
            }
            
          }
          
          
        }
        
        error_calc_tib_aux <- rbind(error_calc_tib_aux,model_error_tib_aux)
        
      } 
      
    }
    
    
    error_calc_tib <-  rbind(error_calc_tib,error_calc_tib_aux) 
    
  }
  
  
  saveRDS(error_calc_tib, file = here("figure_data","runner_error_tib.RDS"))
}else{
  error_calc_tib <- readRDS(file = here("figure_data","runner_error_tib.RDS"))
}

error_calc_tib %>%
  group_by(error_message,model) %>% summarise(n = n_distinct(id)) -> error_summary

error_calc_tib %>%
  filter(prediction == 0 | prediction == Inf) %>%
  pull(id) %>%
  unique() -> bad_prediciton_ids


error_calc_tib %>%
  filter(error_message == "No error") %>%
  group_by(error_message,model) %>% summarise(n = n_distinct(id))

power_law_errors<- tibble()


error_calc_tib %>%
  filter(error_message == "No error" & !id %in% bad_prediciton_ids) %>% 
  mutate(index = paste0(id,duration,distance,power,date)) -> error_calc_tib_indexed


for (m in models[-7]) {
  
  error_calc_tib_indexed %>%
    filter(distance >= 800 & model == m) %>%
    pull(index) %>%
    unique() -> testing_index
  
  
  error_calc_tib_indexed %>%
    filter(model == "Power law" &
             error_message == "No error" &
             distance >= 800 &
             index %in% testing_index) %>%
    mutate(facet = m) -> power_law_errors_aux
  
  power_law_errors <- rbind(power_law_errors,power_law_errors_aux)
  
}




power_law_errors_plot_aux <- power_law_errors %>%
  mutate(error_aux = abs((prediction - duration)/duration)) %>%
  mutate_at(vars(error_aux), ~ . * 100) %>%
  group_by(distance,facet) %>%
  summarise(n = n(),
            error = (1/n)*sum(error_aux),
            se = sqrt((1/n)*var(error_aux))) %>%
  mutate(model = "Power law",
         fill_c = "a") %>%
  mutate_at(c('distance'), as.character) 

power_law_errors %>%
  mutate(distance = "Overall",
         error_aux = abs((prediction - duration)/duration)) %>%
  mutate_at(vars(error_aux), ~ . * 100) %>%
  group_by(distance,facet) %>%
  summarise(n = n(),
            error = (1/n)*sum(error_aux),
            se = sqrt((1/n)*var(error_aux))) %>%
  mutate(model = "Power law",
         fill_c = "a") -> power_law_errors_plot_aux_overall


error_calc_tib_indexed %>%
  filter(model != "Power law" & error_message == "No error" & distance >= 800) %>%
  mutate(error_aux = abs((prediction - duration)/duration)) %>%
  mutate_at(vars(error_aux), ~ . * 100) %>%
  group_by(distance,model) %>%
  summarise(n = n(),
            error = (1/n)*sum(error_aux),
            se = sqrt((1/n)*var(error_aux))) %>%
  mutate(facet = model,
         fill_c = "z") %>%
  mutate_at(c('distance'), as.character) -> plotting_tibble_non_pl

error_calc_tib_indexed %>%
  filter(model != "Power law" & error_message == "No error" & distance >= 800) %>%
  mutate(distance = "Overall",
         error_aux = abs((prediction - duration)/duration)) %>%
  mutate_at(vars(error_aux), ~ . * 100) %>%
  group_by(distance,model) %>%
  summarise(n = n(),
            error = (1/n)*sum(error_aux),
            se = sqrt((1/n)*var(error_aux))) %>%
  mutate(facet = model,
         fill_c = "z")  -> plotting_tibble_non_pl_overall


plotting_tibble <- rbind(plotting_tibble_non_pl,power_law_errors_plot_aux_overall,power_law_errors_plot_aux,plotting_tibble_non_pl_overall) %>%
  mutate(facet = recode(facet,
                        "3CP" = "Three-parameter critical-power",
                        "LJ" = "Luttikholt and Jones (2022)",
                        "PéT" = "Péronnet and Thibault (1989)",
                        "Omni" = "OmPD",
                        "Om3CP"= "Om3CP",
                        "OmExp"= "OmExp"
  ),
  fill_c = recode(fill_c,
                  z = "[See label]",
                  a = "Power law"))





plotting_tibble %>%
  filter(distance != 1609.344) %>%
  pull(distance) %>%
  unique() %>%
  as.character() -> breakss

labels_breaks <- c("800 m", "1500 m", "5 km", "10 km", "Half marathon", "Marathon", "Overall")

dodge_width <- 0.9
alpha_bar <- 0.5
alpha_errorbar <- 1
show.legend <- TRUE


model_labs <- c("Three-parameter critical-power", "Luttikholt and Jones (2022)",
                "Péronnet and Thibault (1989)","OmPD","Om3CP","OmExp")
names(model_labs) <- c("3CP", "LJ","Pét","Om3CP","OmExp","Omni")


plotting_tibble %>%
  filter(distance != 1609.344) %>%
  mutate(fill_c = if_else(model != "Power law","[See label]","Power law")) %>%
  ggplot() + 
  geom_col(mapping = aes(x = distance, y= error, fill = fill_c, group = model), position = position_dodge(width = dodge_width),alpha = alpha_bar,show.legend = show.legend) +
  geom_errorbar(aes(x =distance, ymin=error - se, ymax=error + se, group = model, colour = fill_c), 
                alpha = alpha_errorbar,
                width = 0.5, 
                show.legend = FALSE,
                position = position_dodge(width = dodge_width)) + 
  scale_fill_manual(values = c("#F8766D","#00BFC4")) +
  scale_colour_manual(values = c("#F8766D","#00BFC4")) + 
  scale_x_discrete(expand = c(0, 0),limits = breakss, labels = labels_breaks) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "Event", 
       y = "Average error [\\%]", 
       fill = "Model",
       colour = "Model") + 
  coord_cartesian(ylim = c(0,10)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90,hjust=1,vjust=0.4), strip.text = element_text(size = 8)) +
  facet_wrap(~factor(facet, levels=c("Three-parameter critical-power", "Luttikholt and Jones (2022)",
                                     "Péronnet and Thibault (1989)","OmPD","Om3CP","OmExp")))  -> response_fig



response_fig


