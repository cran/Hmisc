\name{contents}
\alias{contents}
\alias{contents.data.frame}
\alias{print.contents.data.frame}
\alias{html.contents.data.frame}
\title{
Metadata for a Data Frame
}
\description{
\code{contents} is a generic method for which \code{contents.data.frame}
is currently the only method.  \code{contents.data.frame} creates an
object containing the following attributes of the variables 
from a data frame: names, labels (if any), units (if any), number of
factor levels (if any), factor levels,
class, storage mode, and number of NAs.  \code{print.contents.data.frame}
will print the results, with options for sorting the variables.
\code{html.contents.data.frame} creates HTML code for displaying the
results.  This code has hyperlinks so that if the user clicks on the
number of levels the browser jumps to the correct part of a table of
factor levels for all the \code{factor} variables.
}
\usage{
contents(object)
\method{contents}{data.frame}(object)
\method{print}{contents.data.frame}(x,
    sort=c('none','names','labels','NAs'), prlevels=TRUE, \dots) 
\method{html}{contents.data.frame}(object,  sort=c('none','names','labels','NAs'), prlevels=TRUE,
           file=paste('contents',obj$dfname,'html',sep='.'),
           append=FALSE, \dots)
}
\arguments{
  \item{object}{a data frame.  For \code{html} is an object created by
	\code{contents}.}
  \item{x}{an object created by \code{contents}}
  \item{sort}{
	Default is to print the variables in their original order in the
    data frame.  Specify one of 
	\code{"names"}, \code{"labels"}, or \code{"NAs"} to sort the variables by,
	respectively, alphabetically by names, alphabetically by labels, or by
	increaseing order of number of missing values.
  }
  \item{prlevels}{
	set to \code{FALSE} to not print all levels of \code{factor} variables
  }
  \item{file}{file to which to write the html code.  Default is
   \code{"conents.dfname.html"} where \code{dfname} is the name of the data
   frame processed by \code{contents}.}
 \item{append}{set to \code{TRUE} to add html code to an existing file}
 \item{...}{arguments passed from \code{html} to \code{format.df}}
}
\value{an object of class \code{"contents.data.frame"}}

\author{
Frank Harrell
\cr
University of Virginia
\cr
\email{fharrell@virginia.edu}
}
\seealso{
  \code{\link{describe}}, \code{\link{html}}
}
\examples{
set.seed(1)
dfr <- data.frame(x=rnorm(400),y=sample(c('male','female'),400,TRUE))
contents(dfr)
k <- contents(dfr)
print(k, sort='names', prlevels=FALSE)
\dontrun{
html(k)
html(contents(dfr))            # same result
w <- html(k, file='my.html')   # create my.html, don't display
}
}
\keyword{data}
\keyword{interface}

