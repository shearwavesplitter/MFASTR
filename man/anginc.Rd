% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fn_anginc.R
\name{anginc}
\alias{anginc}
\title{Angle of incidence}
\usage{
anginc(tvel, trip)
}
\arguments{
\item{tvel}{Veloctity model read in by readtvel or a stored model (ak135_alp, ak135_taupo)}

\item{trip}{Seismogram triplet (output of readtriplet)}
}
\value{
The angle of incidence at the surface (degrees) and the ray parameter
}
\description{
Determines the angle of incidence for an event
}
\examples{
# Determine the angle of incidence for event 2002.054.09.47.lhor2
pathto <- "~/mfast/sample_data/raw_data"
write_sample(pathto)
event <- "2002.054.09.47.lhor2"
triplet <- readtriplet(event,path=pathto)
a <- anginc(ak135_alp,triplet)
}
