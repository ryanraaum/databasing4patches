% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/peak_bagging.R
\name{gpx_track_to_sf}
\alias{gpx_track_to_sf}
\title{Convert GPX track data frame to sf data frame}
\usage{
gpx_track_to_sf(gpx_track, crs = GPS_CRS)
}
\arguments{
\item{gpx_track}{A GPX track data frame}

\item{crs}{EPSG coordinate reference system number (default is GPS=4326)}
}
\value{
An sf data frame
}
\description{
Convert GPX track data frame to sf data frame
}
