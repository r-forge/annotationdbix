\name{makeDbX}
\Rdversion{1.1}
\alias{makeDbX}
\title{Generates a .dbX annotation database skeleton.}
\description{
	In the following sections the generation of annotation packages from type .dbX is described.
}
\usage{
makeDbX(probeList, organism, species, prefix, outputDir, version, chipName, author, maintainer, manufacturer = "Manufacturer not specified", manufacturerUrl = "ManufacturerUrl not specified", tableName = "sequence", colName = "sequence")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{probeList}{
	Twocolumn list. On the left side probe ids and on the right side a unique identifier. For example the 
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
  \item{outputDir}{
%%     ~~Describe \code{outputDir} here~~
}
  \item{version}{
%%     ~~Describe \code{version} here~~
}
  \item{chipName}{
%%     ~~Describe \code{chipName} here~~
}
  \item{author}{
%%     ~~Describe \code{author} here~~
}
  \item{maintainer}{
%%     ~~Describe \code{maintainer} here~~
}
  \item{manufacturer}{
%%     ~~Describe \code{manufacturer} here~~
}
  \item{manufacturerUrl}{
%%     ~~Describe \code{manufacturerUrl} here~~
}
  \item{tableName}{
%%     ~~Describe \code{tableName} here~~
}
  \item{colName}{
%%     ~~Describe \code{colName} here~~
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
structure(function (probeList, organism, species, prefix, outputDir, 
    version, chipName, author, maintainer, manufacturer = "Manufacturer not specified", 
    manufacturerUrl = "ManufacturerUrl not specified", tableName = "sequence", 
    colName = "sequence") 
standardGeneric("makeDbX"), generic = structure("makeDbX", package = "AnnotationDbiX"), package = "AnnotationDbiX", group = list(), valueClass = character(0), signature = c("probeList", 
"organism", "species", "prefix", "outputDir", "version", "chipName", 
"author", "maintainer"), default = <S4 object of class structure("MethodsList", package = "methods")>, skeleton = function (probeList, 
    organism, species, prefix, outputDir, version, chipName, 
    author, maintainer, manufacturer = "Manufacturer not specified", 
    manufacturerUrl = "ManufacturerUrl not specified", tableName = "sequence", 
    colName = "sequence") 
stop("invalid call in method dispatch to \"makeDbX\" (no default method)", 
    domain = NA)(probeList, organism, species, prefix, outputDir, 
    version, chipName, author, maintainer, manufacturer, manufacturerUrl, 
    tableName, colName), class = structure("standardGeneric", package = "methods"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
