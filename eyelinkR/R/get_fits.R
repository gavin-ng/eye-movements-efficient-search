#' Get individual fits (linear and log)
#'
#' 
#'
#' @param free,fixed Two dataframes: of the data grouped at the individual level -- one for Free, one for Fixed
#' @return A dataframe with fits of each individual
#' @export
#' 
#' 
#' 

get_fits <- function(free, fixed){
  ## Convert set size to numeric
  
  
  free$d_setsize <- as.numeric(as.character(free$d_setsize))
  free$d_setsize_log <- as.numeric(as.character(free$d_setsize_log))
  
  fixed$d_setsize <- as.numeric(as.character(free$d_setsize))
  fixed$d_setsize_log <- as.numeric(as.character(free$d_setsize_log))
  
  ## IDs of good subjects
  good_subs_both <- unique(free$sub_id)
  
  ## create df to input fits
  fits_df <- data.frame(sub_id = rep(NA, length(good_subs_both)),
                          orange_log_free = rep(NA, length(good_subs_both)), 
                          orange_linear_free =  rep(NA, length(good_subs_both)), 
                          blue_log_free =  rep(NA, length(good_subs_both)), 
                          blue_linear_free =  rep(NA, length(good_subs_both)),
                          orange_log_fixed = rep(NA, length(good_subs_both)), 
                          orange_linear_fixed =  rep(NA, length(good_subs_both)), 
                          blue_log_fixed =  rep(NA, length(good_subs_both)), 
                          blue_linear_fixed =  rep(NA, length(good_subs_both)))
  
  
  
  ## Loop to get fits
  for (i in 1:length(good_subs_both)){
    
    fits_df$sub_id[i] <- good_subs_both[i]
    
    fits_df$orange_linear_free[i] <- (summary(lm(meanrt ~ d_setsize, 
                                                   data=(free %>% filter(sub_id == good_subs_both[i] & d_id !=2)))))$r.squared
    
    fits_df$orange_log_free[i] <- (summary(lm(meanrt ~ d_setsize_log, 
                                                data=(free %>% filter(sub_id == good_subs_both[i] & d_id !=2)))))$r.squared
    
    fits_df$blue_linear_free[i] <- (summary(lm(meanrt ~ d_setsize, 
                                                 data=(free %>% filter(sub_id == good_subs_both[i] & d_id !=1)))))$r.squared
    
    fits_df$blue_log_free[i] <- (summary(lm(meanrt ~ d_setsize_log, 
                                              data=(free %>% filter(sub_id == good_subs_both[i] & d_id !=1)))))$r.squared
    
    
    fits_df$orange_linear_fixed[i] <- (summary(lm(meanrt ~ d_setsize, 
                                                    data=(fixed %>% filter(sub_id == good_subs_both[i] & d_id !=2)))))$r.squared
    
    fits_df$orange_log_fixed[i] <- (summary(lm(meanrt ~ d_setsize_log, 
                                                 data=(fixed %>% filter(sub_id == good_subs_both[i] & d_id !=2)))))$r.squared
    
    fits_df$blue_linear_fixed[i] <- (summary(lm(meanrt ~ d_setsize, 
                                                  data=(fixed %>% filter(sub_id == good_subs_both[i] & d_id !=1)))))$r.squared
    
    fits_df$blue_log_fixed[i] <- (summary(lm(meanrt ~ d_setsize_log, 
                                               data=(fixed %>% filter(sub_id == good_subs_both[i] & d_id !=1)))))$r.squared
    
    
    
  }
  
  fits_df1 <- fits_df %>% mutate(or_free = orange_log_free - orange_linear_free, or_fixed = orange_log_fixed - orange_linear_fixed, b_free = blue_log_free - blue_linear_free, b_fixed = blue_log_fixed - blue_linear_fixed)
  # fits_df2 <- fits_df1 %>%
  #   mutate(o_free = if_else(orange_log_free > orange_linear_free, 1,0), 
  #          o_fixed = if_else(orange_log_fixed > orange_linear_fixed, 1,0),
  #          bb_free = if_else(blue_log_free> blue_linear_free,1,0), 
  #          bb_fix = if_else(blue_log_fixed>blue_linear_fixed,1,0)) %>%
  #   select(o_fixed, o_free, bb_free, bb_fix )
  # 
  
  # ccc <- aa %>% group_by(sub_id) %>% summarise(m = (o_free + o_fixed+bb_free+bb_fix))
  # ccc <- ccc %>% summarise(ss = sum(m))
  
  return(fits_df1)
}

