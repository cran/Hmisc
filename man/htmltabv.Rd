% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/htmltabv.r
\name{htmltabv}
\alias{htmltabv}
\title{htmltabc}
\usage{
htmltabv(..., cols = 2, propts = list(quote = FALSE))
}
\arguments{
\item{...}{objects to \code{print()}.  The arguments must be named with the labels you want to print before the verbatim \code{print()}.}

\item{cols}{number of columns in the html table}

\item{propts}{an option list of arguments to pass to the \code{print()} methods; default is to not quote character strings}
}
\value{
character string of html
}
\description{
Simple HTML Table of Verbatim Output
}
\details{
Uses \code{capture.output} to capture as character strings the results of
running \code{print()} on each element of \code{...}.  If an element of \code{...} has
length of 1 and is a blank string, nothing is printed for that cell
other than its name (not in verbatim).
}
\author{
Frank Harrell
}
