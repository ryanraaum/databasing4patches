% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/faster-gpx-reader.R
\name{load_gpx}
\alias{load_gpx}
\title{A stripped down fast GPX file reader}
\usage{
load_gpx(gpxfile)
}
\arguments{
\item{gpxfile}{A path to a GPX file}
}
\value{
A list with a `tracks` sublist
}
\description{
If you want *ONLY* the tracks in a GPX file - no waypoints,
no routes - this reads that in quickly. Or, at least, a LOT
more quickly than the `gpx` package on CRAN.
}
\examples{
gpxfile <- system.file("extdata", "kaaterskill.gpx",
                       package="databasing4patches")
gpxdata <- load_gpx(gpxfile)
gpxdata$tracks[[1]]
}
