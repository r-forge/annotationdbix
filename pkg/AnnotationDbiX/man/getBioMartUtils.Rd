\name{getBioMartUtils}
\alias{getBioMartAttributes}
\alias{getBioMartFilters}
\alias{getBioMartFromXML}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Utilities for bimaRt queries
}
\description{
Helper functions for using the biomaRt interface.
}
\usage{
getBioMartAttributes(x)
getBioMartFilters(x)
getBioMartFromXML(x, results = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
The path and file name to a BioMart XML file created by the XML button at \url{www.biomart.org}.
}
  \item{results}{
If results equal TRUE \command{getBioMartFromXML} returns a result data frame. If results equal FALSE a BioMart object is returned.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
\command{getBioMartAttributes} returns a vector with all selected attributes.
\command{getBioMartFilters} returns a list with all selected filters and their associated values.
\command{getBioMartFromXML} returns a result dataframe if \sQuote{results} equal TRUE else a BioMart object.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
N. Auer \email{norbert@derauer.net}
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
See also \code{\link{makeDbX}}.
}
\examples{
\dontrun{
# returns the results of the BioMart query
# defined in the biomart.xml
BioMartResults <- getBioMartFromXML(x="biomart.xml",
    results=TRUE)
    
# with results = FALSE only the mart is returned
Mart <- getBioMartFromXML(x="biomart.xml",results=FALSE)

# extract the BioMart Attributes from the xml
attributes <- getBioMartAttributes(x="biomart2.xml")
attributes

# extract the BioMart Filters from the xml
filters <- getBioMartFilters(x="biomart".xml3)
filters

# filter values with Entrez Gene ids from the 
# annotation package
filters$values <- unlist(Rkeys(ecoliK12ENTREZID))

# get the results from the BioMart query
BioMartResults <- getBM(mart=Mart,
    attributes=attributes,
    filter=filters$filters,
    value=filters$values)
}}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{IO}
\keyword{utilities}
