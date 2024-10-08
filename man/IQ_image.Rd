% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ph_with_classes.R
\name{IQ_image}
\alias{IQ_image}
\alias{IQ_image.character}
\alias{IQ_image.gg}
\title{Image output for IQRoutputPPTX}
\usage{
IQ_image(x, ...)

\method{IQ_image}{character}(x, pages = 1, ...)

\method{IQ_image}{gg}(x, ...)
}
\arguments{
\item{x}{path to image file or ggplot object}

\item{...}{currently not used}

\item{pages}{integer, the page from the pdf to be extracted}
}
\value{
ggplot object (any table will be printed as image)
}
\description{
Prepares objects which can be interpreted as plots (image files, ggplot) for
output via IQRoutputPPTX.
}
