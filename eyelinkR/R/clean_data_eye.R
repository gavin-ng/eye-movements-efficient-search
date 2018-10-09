#' Get descriptive stats & remove outliers
#'
#' This function loads a file as a matrix. It assumes that the first column
#' contains the rownames and the subsequent columns are the sample identifiers.
#' Any rows with duplicated row names will be dropped with the first one being
#' kepted.
#'
#' @param filename Path to the input file, including extension (.csv)
#' @return A matrix of the infile
#' @export
#' 
#' 
#' 
#' 
#' 

clean_data_eye <- function(eyedata, accuracy_val){
  
  # descrip <- eyedata %>%
  #   group_by(sub_id, condition) %>%
  #   summarise(eye_leave = mean(eye_leave))
  # 
  # all_subs_free <- descrip %>%
  #   filter(condition == 1)
  # 
  # all_subs_fix<- descrip %>%
  #   filter(condition == 2)
  # 
  # all_subs_both <- (all_subs_fix[which(is.element(all_subs_fix$sub_id, all_subs_free$sub_id)),])$sub_id
  # 
  # both_data <- eyedata[which(is.element(eyedata$sub_id, all_subs_both)),]
  # 
  orders <- eyedata %>%
    group_by(sub_id) %>%
    summarise(order = mean(order))
  
  ## remove subjects with accuracy < 90% and (RT < or > 2SD)
  
  
  
  accuracy <- eyedata %>%
    group_by(sub_id, condition) %>%
    summarise(accuracy =mean(hit)) %>%
    filter(accuracy >= accuracy_val) %>%
    distinct()
  
  
  
  
  good_subs_both <- eyedata %>%
    filter(is.element(sub_id, accuracy$sub_id)  & hit ==1) %>%
    group_by(sub_id) %>%
    summarise(RT=mean(RT)) %>%
    mutate(lower_cutoff = mean(RT) - 2*sd(RT), upper_cutoff = mean(RT) + 2*sd(RT)) %>%
    filter(RT > lower_cutoff & RT < upper_cutoff) %>%
    distinct(sub_id)
  
  
  
  orders_1 <- orders[which(is.element(orders$sub_id, good_subs_both$sub_id)),]
  sum(orders_1$order)
  
  
  
  
  
  
  
  
  ###############################################
  ## Create df with good subjects.             ##
  ## Remember to select the viewing condition! ##
  ###############################################
  
  # if (viewing == 'Free'){
  #   good_data <- both_data %>%
  #     filter(is.element(sub_id, good_subs_both) & hit==1)
  #   good_subs_ids <- good_subs_both
  # } else{
  #   good_data <- both_data %>%
  #     filter(is.element(sub_id, good_subs_both) & hit==1)
  #   good_subs_ids <- good_subs_fix
  # }
  
  good_data_eye <- eyedata %>%
    filter(is.element(sub_id, good_subs_both$sub_id) & hit ==1)
  
  good_subs_ids <- good_subs_both
  
  # good_data_$eye_leave <- as.numeric(good_data$eye_leave)
  
  # if (viewing=='Fixed'){
  #   good_data <- good_data %>%
  #     filter(eye_leave==0)
  # }
  
  
  # good_data_fixations <- eyedata_fix %>%
  #   filter(is.element(sub_id, good_subs_free) & hit==1)
  
  return(good_data_eye)
}

