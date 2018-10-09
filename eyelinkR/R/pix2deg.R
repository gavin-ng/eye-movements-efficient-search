#' Converts values from pixels to degrees
#'
#' The coordinates from Eyelink and Dataviewer are in pixels
#' This function converts them to degrees of visual angle 
#' contains the rownames and the subsequent columns are the sample identifiers.
#' Any rows with duplicated row names will be dropped with the first one being
#' kepted.
#'
#' @param coord Coordinates
#' @return A numeric  vector
#' @export
#' 
#' 
pix2deg <- function(coord) {
  (atan((coord/2) / (590*768/300))*2) * (180/pi)
}