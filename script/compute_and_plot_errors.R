library("here")
library("tidyverse")
library("janitor")
library("magrittr")
# library("readxl")


library("tikzDevice")
options( 
  tikzDocumentDeclaration = c(
    "\\documentclass[12pt]{article}",
    "\\usepackage{amssymb, amsmath, graphicx, mathtools, mathdots, stmaryrd}",
    "\\usepackage{tikz}" 
  )
)

# Fit each model and calculate errors.
compare_models_by_error <- function(data) {
    
  data %>% pull(id) %>% unique() -> ID_aux
  models_tbl <- tibble()
    
  for (id_aux in ID_aux) {
    
    #print(id_aux)
    dat <- data %>% filter(id == id_aux)
    powers    <- dat %>% pull(power)
    durations <- dat %>% pull(duration)
    n_powers  <- length(powers)
    # print(powers)
    # print(durations)
    # print(dat)
  
    ##############################################################################
    # Fit the hyperbolic model
    ##############################################################################
    
    # OLS estimate (used for initialising the optimiser):
    ols_hyp <- lm(formula = powers ~ I(1 / durations))
    sigma_ols_hyp_pd <- mean(abs(1 - fitted(ols_hyp) / powers)) # relative errors
  
  
    ############################################################################
    # Fit the power-law model
    ############################################################################
    
    ols_pow <- lm(formula = I(log(powers)) ~ I(log(durations)))
    # # Residual standard error on the backtransformed scale:
    sigma_ols_pow_pd <- mean(abs(1 - exp(fitted(ols_pow)) / powers)) # relative errors
    
    
    ############################################################################
    # Store output
    ############################################################################
    
    models_tbl %<>% bind_rows(
      tibble(
        id             = id_aux,
        sigma_hyp_pd   = sigma_ols_hyp_pd,
        sigma_pow_pd   = sigma_ols_pow_pd,
        sigma_ratio_pd = exp(log(sigma_ols_hyp_pd) - log(sigma_ols_pow_pd)),
        n_powers       = n_powers
      )
    )
  }
  return(models_tbl)
}


# Computes the errors of both models for different durations.
compute_errors <- function(
 data, 
 durations_min,
 durations_max, 
 n_efforts_min = 3,
 n_distances_min = 0
) {
  
  med_duration_min <- max(durations_min)
  med_duration_max <- min(durations_max)
  # Athlete IDs for which we wish to estimate the model
  # (we filter only those athletes who have sufficiently many observations
  # between med_durations_min and med_durations_max of which sufficiently many
  # also need to correspond to different distances)
  data %>%
    filter(duration >= med_duration_min, duration <= med_duration_max) %>%
    group_by(id) %>%
    filter(n() >= n_efforts_min) %>% # remove athletes with less than three observations between 2 and 30 minutes
    filter(n_distinct(distance) >= n_distances_min) %>% 
    ungroup() %>%
    pull(id) %>%
    unique() -> ID
  
  # Axis labels for the boxplots:
  duration_labels <- paste0(durations_min / 60, "--" , durations_max / 60)
  
  # Compute the errors for only medium-duration data:
  models_tbl <- tibble()
  
  for (d in seq_along(durations_min)) {
    models_tbl %<>% 
      bind_rows(
        compare_models_by_error(
          data = filter(
            data, 
            duration >= durations_min[d], 
            duration <= durations_max[d],
            id %in% ID
          )
        ) %>%
        mutate(duration_category = duration_labels[d])
      )
  }
  
  models_tbl %<>%
    select(!sigma_ratio_pd) %>%
    rename("Power law" = sigma_pow_pd, "Hyperbolic" = sigma_hyp_pd) %>%
    pivot_longer(cols = c(`Hyperbolic`, `Power law`), names_to = "model", values_to = "error") %>% 
    mutate(duration = factor(duration_category, levels = duration_labels))  # needed to order the boxplots
  
  # Total number of athletes in the data set:
  data %>% pull(id) %>% unique() %>% length() -> n_athletes
  
  # Number of athletes in the data set who are not filtered out
  data %>% 
    filter(duration >= med_duration_min, duration <= med_duration_max) %>%
    group_by(id) %>%
    filter(n() >= n_efforts_min) %>%
    filter(n_distinct(distance) >= n_distances_min) %>%
    ungroup() %>%
    pull(id) %>%
    unique() -> ID

  length(ID) -> n_athletes_with_sufficiently_many_efforts
  
  data %>% 
    filter(id %in% ID) %>%
    filter(duration < med_duration_min) %>%
    pull(id) %>%
    unique() %>% 
    length() -> n_athletes_with_short_efforts
  
  data %>% 
    filter(id %in% ID) %>%
    filter(duration > med_duration_max) %>%
    pull(id) %>%
    unique() %>% 
    length() -> n_athletes_with_long_efforts
  
  print(paste0("Total number of athletes in the data set: ", n_athletes))
  print(paste0("Total number of athletes in the data set who have at least ", 
               n_efforts_min, 
               " efforts between ", med_duration_min / 60, " and ", med_duration_max / 60, 
               " minutes and who also have at least ", n_distances_min, 
               " distinct distances in this range: ", 
               n_athletes_with_sufficiently_many_efforts))
  print(paste0("Of these, ", n_athletes_with_short_efforts, 
               " athletes have at least one additional effort shorter than ",
               med_duration_min / 60, " minutes, and ", 
               n_athletes_with_long_efforts, 
               " athletes have at least one additional effort longer than ", 
               med_duration_max / 60, " minutes: "))
  return(models_tbl)
}

# Plots the results.
plot_errors <- function(
  data, 
  width = 5, 
  height = 3,
  show.legend = TRUE,
  facet_by_data_source = FALSE
) {
  
  dodge_width <- 0.9
  alpha_bar <- 0.5
  alpha_errorbar <- 1
  
  if (facet_by_data_source) {
    data %<>% group_by(duration, model, data_source)
  } else {
    data %<>% group_by(duration, model)
  }
  data %>% group_by(duration, model) %>%
    summarise(n = n(),
              error_mean = weighted.mean(error, weights = n_powers / sum(n_powers), na.rm = TRUE), 
              # error_sd = 
                # sum(n_powers / sum(n_powers) * (error - error_mean)^2), #sd(error, na.rm = TRUE),
              # error_se = error_sd / sqrt(n)) %>%
              error_se = sd(error, na.rm = TRUE) * 
                sqrt(n - 1) /
                sqrt(n) *
                sqrt(sum(n_powers^2 / sum(n_powers)^2)))  %>%
    ungroup() %>%
    ggplot(mapping = aes(y = 100 * error_mean, x = duration, fill = model)) +
    geom_col(position = position_dodge(width = dodge_width), alpha = alpha_bar, show.legend = show.legend) +
    geom_errorbar(mapping = aes(x = duration, colour = model, 
                                ymin = 100 * (error_mean - error_se), 
                                ymax = 100 * (error_mean + error_se)), 
                  alpha = alpha_errorbar,
                  width = 0.5, 
                  show.legend = FALSE,
                  position = position_dodge(width = dodge_width)) +
    labs(x = "Time to exhaustion [min]", 
         y = "Average error [\\%]", 
         fill = "Model",
         colour = "Model") +
    theme_classic() +
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) -> plot_errorbars
  
  if (facet_by_data_source) {
    plot_errorbars <- plot_errorbars + 
      ggforce::facet_row(vars(data_source), scales = "free_x", space = "free")
  }

  print(plot_errorbars)
}