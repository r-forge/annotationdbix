\name{setIdLink}
\alias{setIdLink}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
sdds
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
setIdLink(x, table, link)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{table}{
%%     ~~Describe \code{table} here~~
}
  \item{link}{
%%     ~~Describe \code{link} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
structure(function (x, table, link) 
standardGeneric("setIdLink"), generic = structure("setIdLink", package = "AnnotationDbiX"), package = "AnnotationDbiX", group = list(), valueClass = character(0), signature = c("x", 
"table", "link"), default = `NULL`, skeleton = function (x, 
    table, link) 
stop("invalid call in method dispatch to \"setIdLink\" (no default method)", 
    domain = NA)(x, table, link), class = structure("standardGeneric", package = "methods"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
