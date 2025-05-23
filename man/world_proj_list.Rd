% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{world_proj_list}
\alias{world_proj_list}
\title{World Map projection list}
\format{
\subsection{"world_proj_list"}{

A list with popular world projections:
\itemize{
\item \code{equal_area}: Projections to keep the area property approximately correct
\itemize{
\item \code{barrel_shape}: Equal-area world map projections with poles represented as lines
\item \code{ellipse}: Equal-area world map projections with poles represented as points
\item \code{rectangular}: cylindrical projection with world map shape like rectangular
}
\item \code{compromise}: Projections compromising shape and area
\itemize{
\item \code{barrel_shape}: pseudocylindrical projection with parallel straight lines at the north and south edges and nearly elliptical lines on the east and west sides
\item \code{rectangular}: cylindrical projection with world map shape like rectangular
}
}
}
}
\source{
\url{https://github.com/ProjectionWizard/projectionwizard.github.io/blob/263b9ff09128e371ee923ab57bfc1ed41bbdc4ba/outputFormat.js#L115}
}
\usage{
world_proj_list
}
\description{
World Map projection list
}
\keyword{datasets}
