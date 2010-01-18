\name{makeDbX}
\alias{makeDbX}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{T6
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
makeDbX(probeList, organism, species, prefix, chipSrc, baseMapTableName, outputDir, version, manufacturer, chipName, manufacturerUrl, author, maintainer)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{probeList}{
%%     ~~Describe \code{probeList} here~~
}
  \item{organism}{
%%     ~~Describe \code{organism} here~~
}
  \item{species}{
%%     ~~Describe \code{species} here~~
}
  \item{prefix}{
%%     ~~Describe \code{prefix} here~~
}
  \item{chipSrc}{
%%     ~~Describe \code{chipSrc} here~~
}
  \item{baseMapTableName}{
%%     ~~Describe \code{baseMapTableName} here~~
}
  \item{outputDir}{
%%     ~~Describe \code{outputDir} here~~
}
  \item{version}{
%%     ~~Describe \code{version} here~~
}
  \item{manufacturer}{
%%     ~~Describe \code{manufacturer} here~~
}
  \item{chipName}{
%%     ~~Describe \code{chipName} here~~
}
  \item{manufacturerUrl}{
%%     ~~Describe \code{manufacturerUrl} here~~
}
  \item{author}{
%%     ~~Describe \code{author} here~~
}
  \item{maintainer}{
%%     ~~Describe \code{maintainer} here~~
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
structure(function (probeList, organism, species, prefix, chipSrc, 
    baseMapTableName, outputDir, version, manufacturer, chipName, 
    manufacturerUrl, author, maintainer) 
standardGeneric("makeDbX"), generic = structure("makeDbX", package = "AnnotationDbiX"), package = "AnnotationDbiX", group = list(), valueClass = character(0), signature = c("probeList", 
"organism", "species", "prefix", "chipSrc", "baseMapTableName", 
"outputDir", "version", "manufacturer", "chipName", "manufacturerUrl", 
"author", "maintainer"), default = <S4 object of class structure("MethodsList", package = "methods")>, skeleton = function (probeList, 
    organism, species, prefix, chipSrc, baseMapTableName, outputDir, 
    version, manufacturer, chipName, manufacturerUrl, author, 
    maintainer) 
stop("invalid call in method dispatch to \"makeDbX\" (no default method)", 
    domain = NA)(probeList, organism, species, prefix, chipSrc, 
    baseMapTableName, outputDir, version, manufacturer, chipName, 
    manufacturerUrl, author, maintainer), class = structure("standardGeneric", package = "methods"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
