\name{addNewAnnotation}
\Rdversion{1.1}
\alias{addNewAnnotation}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
addNewAnnotation(x, data, newTableName, data.colNames, mapTableName)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{newTableName}{
%%     ~~Describe \code{newTableName} here~~
}
  \item{data.colNames}{
%%     ~~Describe \code{data.colNames} here~~
}
  \item{mapTableName}{
%%     ~~Describe \code{mapTableName} here~~
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
structure(function (x, data, newTableName, data.colNames, mapTableName) 
standardGeneric("addNewAnnotation"), generic = structure("addNewAnnotation", package = "AnnotationDbiX"), package = "AnnotationDbiX", group = list(), valueClass = character(0), signature = c("x", 
"data", "newTableName", "data.colNames", "mapTableName"), default = <S4 object of class structure("MethodsList", package = "methods")>, skeleton = function (x, 
    data, newTableName, data.colNames, mapTableName) 
stop("invalid call in method dispatch to \"addNewAnnotation\" (no default method)", 
    domain = NA)(x, data, newTableName, data.colNames, mapTableName), class = structure("standardGeneric", package = "methods"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
