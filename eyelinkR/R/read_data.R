#' Read an Eyelink fixation report
#'
#' This function loads a file as a matrix. It assumes that the first column
#' contains the rownames and the subsequent columns are the sample identifiers.
#' Any rows with duplicated row names will be dropped with the first one being
#' kepted.
#'
#' @param filename Path to the input file, including extension (.csv)
#' @return A matrix of the infile
#' @export

read_data <- function(filename, expt=NULL){
  df <- read.csv(filename, header=TRUE, stringsAsFactors=FALSE, row.names=NULL)
  df <- df[which(df$CHECK_RECALIBRATION == "0" ),]
  
  # Extract relevant columns
  sub_id <- df$RECORDING_SESSION_LABEL
  saccade_latency <- as.numeric(df$PREVIOUS_SAC_START_TIME) - (as.numeric(df$ARRAY_TIME) - as.numeric(df$X__TRIAL_START_TIME__1))
  saccade_amplitude <- as.numeric(as.character(df$PREVIOUS_SAC_AMPLITUDE))
  fix_x <- df$CURRENT_FIX_X -1024/2
  fix_y <- -(df$CURRENT_FIX_Y -768/2)
  fix_duration <- df$CURRENT_FIX_DURATION
  fix_ok <- if_else(df$CURRENT_FIX_START - (as.numeric(as.character(df$ARRAY_TIME)) - 
                                              as.numeric(as.character(df$X__TRIAL_START_TIME__1))) > 0, 1,0)
  trial <- df$trial
  block <- df$block
  RT <- as.numeric(df$KEYBOARD_PRESS_TIME) - as.numeric(df$ARRAY_TIME)
  target_key <- df$target_key
  response <- df$KEYBOARD_RESPONSE_KEY
  d_id <- df$d_id
  d_setsize <- as.numeric(df$d_setsize) + 1
  d_setsize_log <- log(d_setsize)
  eye_leave <- df$DID_EYE_LEAVE
  t_xloc <- as.numeric(df$t_xloc) -1024/2
  t_yloc <- 0 -(as.numeric(df$t_yloc) -768/2)
  quad <- df$quadrant
  quartile <- df$quartile
  circle <- df$circle
  recalibration <- df$CHECK_RECALIBRATION
  order <- df$order ## 0 = fixed first, 1 = free first
  
  
  ## Check condition; 1 = free block
  check_condition <- function(x, y){
    if((x=="1" & y=="1") | (x=="0" & y =="2")){ 
      return(1)
    } else{
      return (2)
    }
  }
  
  condition_try <- function(x, y){
    tryCatch(condition <- mapply(check_condition, x, y),
             error=function(e) {
               condition <- if_else(y==1,1,2)
             })
    
  }
  
  condition <- condition_try(df$order, df$block)
  
  
  
  
  ## Combine relevant columns into single df
  all_df <- data.frame(cbind(sub_id, trial, block, saccade_latency, fix_ok, saccade_amplitude, 
                             fix_x, fix_y, t_xloc, t_yloc, fix_duration, RT, target_key, 
                             response, d_id, d_setsize, d_setsize_log, eye_leave, quad, circle, quartile, 
                             recalibration, order, condition))
  
  all_df$sub_id <- sub_id
  
  all_df <- lapply(all_df, as.character)
  all_df <- data.frame(all_df, stringsAsFactors = FALSE)
  # all_df[, c(1:12,15:24)] <- lapply(all_df[, c(1:12, 15:24)], as.numeric)
  all_df$hit <- as.character(all_df$target_key) == as.character(all_df$response)
  all_df <- data.frame(lapply(all_df, as.numeric))
  
  all_data <- data.frame()
  dnc <- data.frame()
  for (i in unique(all_df$sub_id)){
    
    # first_saccades <- all_df[which(all_df$sub_id == i & all_df$saccade_latency > 50 & all_df$eye_leave==1),]
    first_saccades <- all_df %>% filter(sub_id == i & saccade_latency >0)
    first_saccades <- first_saccades[which(!duplicated(first_saccades$trial)),]
    
    
    sub_df <- all_df[which( all_df$sub_id == i  ),]
    total_fixations <- (sub_df %>% 
                           group_by(trial) %>%
                           summarise(fixations = sum(fix_ok)))$fixations
    
    
    sub_df <- sub_df[which(!duplicated(sub_df$trial)),]
    sub_x <- sub_df$fix_x
    sub_y <- sub_df$fix_y
  
    
    sub_df$saccade_latency <- first_saccades$saccade_latency[pmatch(sub_df$trial, first_saccades$trial)]
    sub_df$fix_x <- first_saccades$fix_x[pmatch(sub_df$trial, first_saccades$trial)]
    sub_df$fix_y <- first_saccades$fix_y[pmatch(sub_df$trial, first_saccades$trial)] 
    sub_df$fixations <- total_fixations +1
    
    # for (j in 1:nrow(sub_df)){
    #   if(is.na(sub_df$fix_x[j])){
    #     sub_df$fix_x[j] <- sub_x[j]
    #     sub_df$fix_y[j] <- sub_y[j]
    #   }
    #   
    # }
    
    
    # 
    # all_sac_trials <- all_df[which(all_df$saccade_latency > 50 & all_df$eye_leave==1),]
    # all_fix_trials <- all_df %>% filter(fix_ok ==1)
    # 
    
    if (expt=='pilot'){
      trial_min = 478
    }else{
      trial_min = 403
    }
    if (nrow(sub_df) < trial_min){
      dnc <- rbind(dnc, i)
      next
    }
    #
    # if (nrow(sub_df) < 478){
    #   dnc <- rbind(dnc, i)
    #   next
    # }
    
    all_data <- rbind(all_data, sub_df)
  }
  
  # all_data_sac <- all_df[which(all_df$saccade_latency >50 & all_df$eye_leave == 1),]
  
  
  all_sac_trials <- all_df %>% 
    filter(saccade_latency > 50 & eye_leave ==1)
  all_fix_trials <- all_df %>%
    filter(fix_ok == 1)
  
  # total_fix <- all_df %>%
  #   group_by(sub_id, trial) %>%
  #   summarise(fixations = sum(fix_ok))
  # 
  # all_data$fixations <- all_df %>%
  #   group_by(sub_id, trial) %>%
  #   summarise(fixations = sum(fix_ok))
  
  all_data$half <- lapply(all_data$quartile, function(x){
    if (as.numeric(x)>2){
      2
    } else {
      1
    }
    
  })
  
  euclidean <- function(x1, x2, y1, y2){
  
    sqrt((x1-x2)^2 + (y1-y2)^2)
    
    
  }
  
  all_data$distance <- mapply(euclidean, all_data$fix_x, all_data$t_xloc, all_data$fix_y, all_data$t_yloc)
  all_data$distance <- pix2deg(all_data$distance)
  
  


  
  all_data$eccentricity <- lapply(all_data$circle, function(x){
    if(x==1){
      return(4.2)
    } else if(x==2){
      return(7.4)
    } else{
      return(14.3)
    }
  })
  
  all_data$eccentricity <- as.numeric(all_data$eccentricity)
  
  return(list(all_data, all_sac_trials, all_fix_trials))
  
}




