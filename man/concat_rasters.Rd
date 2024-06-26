% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/concat_rasters.R
\name{concat_rasters}
\alias{concat_rasters}
\title{Concatenate Raster Layers}
\usage{
concat_rasters(rasters)
}
\arguments{
\item{rasters}{A \code{terra} raster object containing multiple raster layers.}
}
\value{
A single \code{terra} raster object containing all concatenated layers.
}
\description{
This function concatenates all raster layers within a \code{terra} raster object
into a single raster object.
}
