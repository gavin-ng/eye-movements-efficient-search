#' Calculate mean without zeros
#'
#' Takes a numeric vector (???) and first extracts all the zeros
#' If the vector consists of all zeros, return zero
#' If not, calculate the mean by excluding the zeros
#' Returns a numeric value
#'
#' @param nums Path to the input file
#' @return A numeric
#' @export


nzmean <- function(nums) {
  zvals <- nums==0
  if (all(zvals)){
    0
  } else{
    mean(nums[!zvals])
  }

}