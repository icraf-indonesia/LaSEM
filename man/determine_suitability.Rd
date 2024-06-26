% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/determine_suitability.R
\name{determine_suitability}
\alias{determine_suitability}
\title{Determine Suitability and Limiting Factors}
\usage{
determine_suitability(class_vector)
}
\arguments{
\item{class_vector}{A vector of characters representing suitability classes.
The classes are expected to be "S1", "S2", "S3", or "N", with "N" being
the lowest and "S1" the highest suitability class.}
}
\value{
A list containing two elements: 'suitability', which is the lowest
suitability class found in the input vector, and 'limiting_factor',
which is a vector of positions where this class occurs in the input.
If the highest class is "S1", 'limiting_factor' is set to NA.
}
\description{
This function evaluates a vector of suitability classes and determines the
lowest suitability class based on a predefined priority order. It also
identifies the positions of this class within the vector.
}
\examples{
determine_suitability(c("S1", "S1", "S3", "S3", "S2"))

}
