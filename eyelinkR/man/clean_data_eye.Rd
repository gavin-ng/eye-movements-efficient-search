% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clean_data_eye.R
\name{clean_data_eye}
\alias{clean_data_eye}
\title{Get descriptive stats & remove outliers}
\usage{
clean_data_eye(eyedata, accuracy_val)
}
\arguments{
\item{filename}{Path to the input file, including extension (.csv)}
}
\value{
A matrix of the infile
}
\description{
This function loads a file as a matrix. It assumes that the first column
contains the rownames and the subsequent columns are the sample identifiers.
Any rows with duplicated row names will be dropped with the first one being
kepted.
}
