

calculate_percentiles <- function(file_paths,percentile){
  
  percentiles_aux <- tibble()
  
  for (i in 1:length(file_paths)){
    
    rider_i_file_path <- list.files(file_paths[i])
    
    
    if(length(rider_i_file_path) > 0){
      
      rider_i_data <- read_rds(file.path(file_paths[i],rider_i_file_path[1]))
      
      rider_i_aux <- rider_i_data %>%
        filter(mmp_value < 2000 & mmp_value > 0) %>%
        group_by(duration) %>%
        slice(which.max(mmp_value))
      
      percentiles_aux <- rbind(percentiles_aux,rider_i_aux)}
  }
  
  percentiles_aux %>% 
    group_by(duration) %>%
    summarise(quant = quantile(mmp_value, probs = percentile/100))
  
}


clean_gc_data <- function(file_paths, percentile){
  
  gc_analysis_data <- tibble()
  
  for (i in 1:length(file_paths)){
    rider_i_file_path <- list.files(file_paths[i])
    if(length(rider_i_file_path) > 0){
      
      rider_i_data <- read_rds(file.path(file_paths[i],rider_i_file_path[1])) %>%
        filter(mmp_value < 2000 & mmp_value > 0) %>%
        left_join(percentile, by="duration") %>% 
        mutate(rm_ind = if_else(mmp_value < quant,0,1)) %>%
        select(-quant)
      
      # rider_i_no_rides_removed_aux <- length(unique(rider_i_data %>%
      #                                                 filter(rm_ind == 1)))
      # rides_rm_tibble <- rider_i_mmps %>% 
      #      filter(rm_ind == 1)
      # rides_rm <- unique(rides_rm_tibble$ride_id)
      # rider_i_no_rides_removed_aux <- length(rides_rm)
      
        rider_i_no_rides_removed_aux <- rider_i_data %>%
          filter(rm_ind == 1)
        rider_i_no_rides_removed <- length(unique(rider_i_no_rides_removed_aux$ride_id))

        if(rider_i_no_rides_removed > 0){
        rides_rm_tibble <- rider_i_data %>%
          filter(rm_ind == 1)
        rides_rm <- unique(rides_rm_tibble$ride_id)
        rides_all <- unique(rider_i_data$ride_id)
        rides_not_rm <- rides_all[! rides_all %in% rides_rm]
        rider_i_clean_aux <- rider_i_data %>%
          filter(ride_id %in% rides_not_rm)
        }else{rider_i_clean_aux <- rider_i_data}
      
      
      
      
      
      
      # if(rider_i_no_rides_removed_aux > 0){
      #   rides_rm_aux <- rider_i_data %>% 
      #     filter(rm_ind == 1)
      #   rides_rm <- unique(rides_rm$ride_id)
      #   rides_all <- unique(rider_i_data$ride_id) 
      #   rides_not_rm <- rides_all[! rides_all %in% rides_rm]
      #   rider_i_clean_aux <- rider_i_data %>%
      #     filter(ride_id %in% rides_not_rm)
      # }else{rider_i_clean_aux <- rider_i_data}
      
      
      rider_i_clean <- rider_i_clean_aux  %>%
        group_by(duration) %>%
        slice(which.max(mmp_value))
      
      
      gc_analysis_data <- rbind(gc_analysis_data,rider_i_clean) 
        
      
    }
  }
  
  gc_analysis_data %>% 
    group_by(rider_id) %>%
    filter(lead(mmp_value, offset = 1) < mmp_value) %>%
    filter(lead(mmp_value, offset = 1) < mmp_value)
}