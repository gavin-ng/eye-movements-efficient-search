% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pix2deg.R
\name{pix2deg}
\alias{pix2deg}
\title{Converts values from pixels to degrees}
\usage{
pix2deg(coord)
}
\arguments{
\item{coord}{Coordinates}
}
\value{
A numeric  vector
}
\description{
The coordinates from Eyelink and Dataviewer are in pixels
This function converts them to degrees of visual angle 
contains the rownames and the subsequent columns are the sample identifiers.
Any rows with duplicated row names will be dropped with the first one being
kepted.
}
