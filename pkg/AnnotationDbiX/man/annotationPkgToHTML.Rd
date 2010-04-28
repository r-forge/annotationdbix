\name{annotationPkgToHTML}
\Rdversion{1.1}
\alias{annotationPkgToHTML}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
annotationPkgToHTML(x, caption, outputDir, mainTable, tables = character(), filter = character(), onlyIDs = FALSE, extdata = NULL, colOrder = NULL, css = "", tableRows = 50, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{caption}{
%%     ~~Describe \code{caption} here~~
}
  \item{outputDir}{
%%     ~~Describe \code{outputDir} here~~
}
  \item{mainTable}{
%%     ~~Describe \code{mainTable} here~~
}
  \item{tables}{
%%     ~~Describe \code{tables} here~~
}
  \item{filter}{
%%     ~~Describe \code{filter} here~~
}
  \item{onlyIDs}{
%%     ~~Describe \code{onlyIDs} here~~
}
  \item{extdata}{
%%     ~~Describe \code{extdata} here~~
}
  \item{colOrder}{
%%     ~~Describe \code{colOrder} here~~
}
  \item{css}{
%%     ~~Describe \code{css} here~~
}
  \item{tableRows}{
%%     ~~Describe \code{tableRows} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
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
structure(function (x, caption, outputDir, mainTable, tables = character(), 
    filter = character(), onlyIDs = FALSE, extdata = NULL, colOrder = NULL, 
    css = "", tableRows = 50, ...) 
standardGeneric("annotationPkgToHTML"), generic = structure("annotationPkgToHTML", package = "AnnotationDbiX"), package = "AnnotationDbiX", group = list(), valueClass = character(0), signature = c("x", 
"caption", "outputDir", "mainTable"), default = <S4 object of class structure("MethodsList", package = "methods")>, skeleton = function (x, 
    caption, outputDir, mainTable, tables = character(), filter = character(), 
    onlyIDs = FALSE, extdata = NULL, colOrder = NULL, css = "", 
    tableRows = 50, ...) 
stop("invalid call in method dispatch to \"annotationPkgToHTML\" (no default method)", 
    domain = NA)(x, caption, outputDir, mainTable, tables, filter, 
    onlyIDs, extdata, colOrder, css, tableRows, ...), class = structure("standardGeneric", package = "methods"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
