#' Create a dataframe that is ready for ggplot
#'
#'
#' @param eye_var,col_var,out_var,n df with the indiidual data, name of the column of interest, name of output column, number of cases
#' @return A df
#' @export
#' 
#' 
#' 
#' 
#' 
#' 
#' 


df_plot <- function(eye_var, col_var, out_var, n){
  
  df <- eye_var %>% filter(!is.na(col_var)) %>%
    group_by(d_id, d_setsize) %>%
    summarise(out_var = mean(col_var), sem = sd(col_var)/sqrt(n))
  
  return(df)
  
  
}

