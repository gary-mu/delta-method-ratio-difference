#When you have test and control group aggregate stats and want to calculate
#their ratio difference (relative % difference) and construct a confidence interval

pct_delta_var <- delta_method_ratio_variance(
  exp_agg_data$cancellation_rate_mean[exp_agg_data$group == 'test'],
  exp_agg_data$cancellation_rate_mean[exp_agg_data$group == 'control'],
  exp_agg_data$cancellation_rate_se[exp_agg_data$group == 'test'],
  exp_agg_data$cancellation_rate_se[exp_agg_data$group == 'control']
  )

pct_relative_delta_difference <- function(test_mean, control_mean, test_se, control_se, test_sample_size, control_sample_size, confidence_level){
  pct_delta <- test_mean/control_mean -1
  pct_delta_var <- delta_method_ratio_variance(test_mean, control_mean, test_se, control_se)
  t_stats <- qt(1-((1-0.95)/2), (test_sample_size + control_sample_size -2))
  upper_ci <- pct_delta + t_stats * sqrt(pct_delta_var)
  lower_ci <- pct_delta - t_stats * sqrt(pct_delta_var)
  
  stat_data <- c(confidence_level, 
                 paste0(round(pct_delta, 4)*100, '%'),
                 paste0(round(upper_ci, 4)*100, '%'), 
                 paste0(round(lower_ci, 4)*100, '%'), 
                 paste0(round(sqrt(pct_delta_var), 4)*100, '%'))
  names(stat_data) <- c('Confidence level', 
                        '% delta (relative)', 
                        '% delta upper limit', 
                        '% delta lower limit', 
                        '% delta se')
  return(stat_data)
}

#example: 
#test group: sample size 164530, mean: 0.0045, standard_error: 0.000167
#control group: sample size 164457, mean: 0.0138, standard_error: 0.000287
#pct_relative_delta_difference(
#  0.0045,
#  0.0138,
#  0.000167,
#  0.000287,
#  164530,
#  164457
#  '0.95'
#)

#    Confidence level   % delta (relative)   % delta upper limit   % delta lower limit      % delta se 
#             "0.95"           "-66.68%"           "-63.95%"           "-69.42%"              "1.4%" 
