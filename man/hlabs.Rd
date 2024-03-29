% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hlab.r
\name{hlabs}
\alias{hlabs}
\title{hlabs}
\usage{
hlabs(x, y, html = FALSE)
}
\arguments{
\item{x}{a single variable name, unquoted}

\item{y}{a single variable name, unquoted}

\item{html}{set to \code{TRUE} to render in html (for \code{plotly}), otherwise the result is \code{plotmath} expressions}
}
\value{
result of \code{\link[ggplot2:labs]{ggplot2::labs()}}
}
\description{
Front-end to ggplot2 labs Function
}
\details{
Runs \code{x}, \code{y}, or both through \code{\link[=hlab]{hlab()}} and passes the constructed labels to the \link[ggplot2:labs]{ggplot2::labs} function to specify x- and y-axis labels specially formatted for units of measurement
}
\examples{
# Name the current dataset d, or specify a name with
# options(curr_ds='...') or run `extractlabs`, then
# ggplot(d, aes(x,y)) + geom_point() + hlabs(x,y)
# to specify only the x-axis label use hlabs(x), or to
# specify only the y-axis label use hlabs(y=...)
}
\author{
Frank Harrell
}
