#When you have test and control group aggregate stats and want to calculate
#their ratio difference (relative % difference) and construct a confidence interval
#https://www.stat.rice.edu/~dobelman/notes_papers/math/TaylorAppDeltaMethod.pdf
delta_method_ratio_variance <- function(mean_test, mean_control, se_test, se_control){
  var_calc <- (se_test^2 * mean_control^2 + se_control^2 * mean_test^2)/mean_control^4
  return(var_calc)
}

#Another deriviation of ratio delta variance calculation
#delta_method_ratio_variance2 <- function(test_mean, control_mean, test_se, control_se){
#  ratio_variance <- (test_se ^ 2 +  (test_mean/control_mean)^2 * control_se^2)/ control_mean^2
#  return(ratio_variance)
#}



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
