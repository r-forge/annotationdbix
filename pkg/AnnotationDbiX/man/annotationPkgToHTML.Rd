\name{annotationPkgToHTML}
\Rdversion{1.1}
\alias{annotationPkgToHTML}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Generates a nice to read HTML table from the annotation package
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
annotationPkgToHTML(x, caption, outputName, mainTable, tables = character(), filter = character(), onlyIDs = FALSE, extdata = NULL, colOrder = NULL, css = "", tableRows = 50)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
The path and filename, a SQLiteConnection object to the extended annotation database of interest or a Bimap object of class \emph{AnnDbBimap}.
}
  \item{caption}{
The titel of the HTML page.
}
  \item{outputName}{
The path and filename of the HTML page.
}
  \item{mainTable}{
The table name for what the HTML table should be aligned. Must be also defined in the \sQuote{tables} parameter. If \sQuote{x} is from type \emph{AnnDbBimap} this parameter is missing.
}
  \item{tables}{
All tables which should be included in the HTML page except the \sQuote{mainTable} table.
}
  \item{filter}{
A vector of identifiers for what the table should be filtered. The identifiers must be from the same type like the \sQuote{mainTable} parameter is.
}
  \item{onlyIDs}{
If TRUE only the main column of a table without its attribute columns is printed in the HTML page.
}
  \item{extdata}{
A list of type dataframe with external data which should printed in the HTML page. In the first column are identifiers from type \sQuote{mainTable} in the other columns are the external data which is paired with the first column. The column names of the table are also used as column names in the HTML table.
}
  \item{colOrder}{
A vector of table names in the order as the columns should be arranged in the HTML table. For the \sQuote{extdata} table the column names are used in the order vector.
}
  \item{css}{
The path and filename of a external cascading style sheet file which should be used instead of the internal used css. If css is equal NULL no style sheet is used. TODO ids for css
}
  \item{tableRows}{
Number of main table rows per html page.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
If the function succeeds HTML pages were produced.
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
library(ecoliK12.db)

annotationPkgToHTML(ecoliK12_dbconn(),
    caption='E. Coli K12 microarray chip',
    outputName='ecoliK12_1',
    mainTable="probes",
    tables=c("ec","genes","kegg","blattner"),
    filter=Lkeys(ecoliK12SEQUENCE)[1:5],
    colOrder=c("probes","blattner","genes","kegg","ec"))

# creates a HTML page with a Bimap object
annotationPkgToHTML(ecoliK12SEQUENCE,
    caption='E. Coli K12 microarray chip',
    outputName='ecoliK12_2',
    filter=Lkeys(ecoliK12SEQUENCE)[1:5])
}}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{IO}
\keyword{utilities}
