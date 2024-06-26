% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/process_suitability.R
\name{process_suitability}
\alias{process_suitability}
\title{Process Suitability Factors for Crop Suitability Analysis}
\usage{
process_suitability(suitability_factors, crop_suitability)
}
\arguments{
\item{suitability_factors}{A SpatRaster object from the terra package representing environmental factors
affecting crop growth such as soil quality, climate conditions, etc.}

\item{crop_suitability}{A dataframe with crop suitability parameters, each row representing a different
parameter and its associated suitability conditions.}
}
\value{
A list containing four elements:
\itemize{
\item{suitability_raster}{A SpatRaster object representing the combined suitability analysis.}
\item{suitability_polygon}{An sf object representing suitability areas as polygons.}
\item{suitability_attr}{A dataframe containing attributes for each suitability category.}
\item{suitability_by_factors}{A list of SpatRaster objects for individual suitability factors.}
}
}
\description{
This function integrates environmental suitability factors with crop suitability parameters to produce
a comprehensive analysis of crop suitability. It involves reclassifying raster layers based on
suitability parameters, creating a frequency table, and converting rasters to spatial polygons.
This function relies on specific functions from terra, dplyr, tidyr, and sf packages.
}
\examples{
\dontrun{
  # Assuming suitability_factors and crop_suitability are already defined
  suitability_results <- process_suitability(suitability_factors, crop_suitability)
  View(suitability_results$suitability_raster)
  View(suitability_results$suitability_polygon)
  View(suitability_results$suitability_attr)
  # Explore individual factor suitability rasters
  lapply(suitability_results$suitability_by_factors, View)
}
}
