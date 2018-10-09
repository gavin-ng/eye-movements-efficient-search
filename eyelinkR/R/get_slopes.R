#' Get individual slopes (linear and log)
#'
#' 
#'
#' @param free,fixed Two dataframes: of the data grouped at the individual level -- one for Free, one for Fixed
#' @return A dataframe with slopes of each individual
#' @export
#' 
#' 
#' 

get_slopes <- function(free, fixed){
  ## Convert set size to numeric
  
  
  free$d_setsize <- as.numeric(as.character(free$d_setsize))
  free$d_setsize_log <- as.numeric(as.character(free$d_setsize_log))
  
  fixed$d_setsize <- as.numeric(as.character(free$d_setsize))
  fixed$d_setsize_log <- as.numeric(as.character(free$d_setsize_log))
  
  ## IDs of good subjects
  good_subs_both <- unique(free$sub_id)
  
  ## create df to input slopes
  slopes_df <- data.frame(sub_id = rep(NA, length(good_subs_both)),
                          orange_log_free = rep(NA, length(good_subs_both)), 
                          orange_linear_free =  rep(NA, length(good_subs_both)), 
                          blue_log_free =  rep(NA, length(good_subs_both)), 
                          blue_linear_free =  rep(NA, length(good_subs_both)),
                          orange_log_fixed = rep(NA, length(good_subs_both)), 
                          orange_linear_fixed =  rep(NA, length(good_subs_both)), 
                          blue_log_fixed =  rep(NA, length(good_subs_both)), 
                          blue_linear_fixed =  rep(NA, length(good_subs_both)))
  
  
  
  ## Loop to get slopes
  for (i in 1:length(good_subs_both)){
    
    slopes_df$sub_id[i] <- good_subs_both[i]
    
    slopes_df$orange_linear_free[i] <- (summary(lm(meanrt ~ d_setsize, 
                                                   data=(free %>% filter(sub_id == good_subs_both[i] & d_id !=2)))))$coefficients[2]
    
    slopes_df$orange_log_free[i] <- (summary(lm(meanrt ~ d_setsize_log, 
                                                data=(free %>% filter(sub_id == good_subs_both[i] & d_id !=2)))))$coefficients[2]
    
    slopes_df$blue_linear_free[i] <- (summary(lm(meanrt ~ d_setsize, 
                                                 data=(free %>% filter(sub_id == good_subs_both[i] & d_id !=1)))))$coefficients[2]
    
    slopes_df$blue_log_free[i] <- (summary(lm(meanrt ~ d_setsize_log, 
                                              data=(free %>% filter(sub_id == good_subs_both[i] & d_id !=1)))))$coefficients[2]
    
    
    slopes_df$orange_linear_fixed[i] <- (summary(lm(meanrt ~ d_setsize, 
                                                    data=(fixed %>% filter(sub_id == good_subs_both[i] & d_id !=2)))))$coefficients[2]
    
    slopes_df$orange_log_fixed[i] <- (summary(lm(meanrt ~ d_setsize_log, 
                                                 data=(fixed %>% filter(sub_id == good_subs_both[i] & d_id !=2)))))$coefficients[2]
    
    slopes_df$blue_linear_fixed[i] <- (summary(lm(meanrt ~ d_setsize, 
                                                  data=(fixed %>% filter(sub_id == good_subs_both[i] & d_id !=1)))))$coefficients[2]
    
    slopes_df$blue_log_fixed[i] <- (summary(lm(meanrt ~ d_setsize_log, 
                                               data=(fixed %>% filter(sub_id == good_subs_both[i] & d_id !=1)))))$coefficients[2]
    
    
    
  }
  
  return(slopes_df)
  
}

