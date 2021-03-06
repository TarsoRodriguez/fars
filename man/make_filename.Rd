% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{make_filename}
\alias{make_filename}
\title{Creates the name of file}
\usage{
make_filename(year)
}
\arguments{
\item{year}{The year of the required information.}
}
\value{
This function returns the file name that should be loaded.
}
\description{
This function builds a string with the name of file that will be loaded based on the year of the required information.
}
\examples{
\dontrun{
make_filename(2013)
}

}
