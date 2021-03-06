% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/harvest.area.r
\name{harvest.area}
\alias{harvest.area}
\title{Area-based clear and partial cuts}
\usage{
harvest.area(land, params, cc.area, pc.area, km2.pixel)
}
\arguments{
\item{land}{A \code{landscape} data frame with forest stand records in rows}

\item{params}{A list of default parameters generated by the function \code{default.params()} or 
a customized list of model parameters}

\item{cc.area}{A data frame with the number of cells to clear cut per mgmt unit returned by the function \code{timber.area()}}

\item{pc.area}{A data frame with the number of cells to partial cut per mgmt unit returned by the function \code{timber.partial.area()}}

\item{km2.pixel}{Area in km^2^ of grid cells}
}
\value{
A list of four items: 
 \itemize{
 \item{\code{cc.cells}: A vector with the \code{cell.id} of the clear cut cells}
 \item{\code{pc.cells}: A vector with the \code{cell.id} of the partial cut cells}
 \item{\code{track.cut}: A data frame with managed areas by different prescriptions}
 \item{\code{spp.track}: A data frame with the area and volume clear-cut and partial cut per species}
 }
}
\description{
Selects cells for clear and partial cuts on an area-based
}
\examples{
data(landscape)
params = default.params()
cc.area = timber.area(landscape, params) 
pc.area = timber.partial.area(landscape, params, pc.step=5) 
harvest = harvest.area(landscape, params, cc.area, pc.area, km2.pixel=4)

}
