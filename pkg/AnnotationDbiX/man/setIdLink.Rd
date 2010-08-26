\name{setIdLink}
\alias{setIdLink}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Adds a WWW address to an associated database column
}
\description{
The \command{setIdLink()} function is important for the \command{annotationPkgToHTML()} function which generates HTML pages with the package annotations . Each of these annotation identifiers can be visualised in a HTML page as only an identifier string or as an identifier reference linked to an associated web database. The \command{setIdLink()} function adds this address to the database. This address can also be set in the \command{makeDbX()} and \command{addNewAnnotation()} function as a parameter.
}
\usage{
setIdLink(x, table, link)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
The path and file name or a \strong{SQLiteConnection} object to an extended \emph{.db} SQLite database of interest.
}
  \item{table}{
The Name of the database table in the annotation package.
}
  \item{link}{
A valid WWW address. If the specific identifier string of the main column is included in the address then this part of the address is replaced by the string \command{\sQuote{$ID}}. This placeholder would be replaced by the actual identifier string when the \command{annotationPkgToHTML()} function generates the HTML page. For example the web address \url{<http://www.genome.jp/dbget-bin/www_bget?eco:$ID>} is replaced by \url{<http://www.genome.jp/dbget-bin/www_bget?eco:b1362>} for the Locus Link identifier \sQuote{b1362}.

If a table has more than one columns except the internal identifier column for each column an address could be defined. The addresses are separated by a | character and have the same order as the columns in the table. For all columns the same identifier from the main table is used as the placeholder string.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
	TRUE, if the function succeeds.
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
See also \command{annotationPkgToHTML()} function.
}
\examples{
\dontrun{
# loading an extended annotation package
library(ecoliK12.db)

# the identifier string was replaced by '$ID'
setIdLink(ecoliK12_dbconn(),"blattner",
    "http://www.genome.jp/dbget-bin/www_bget?eco:$ID")
}}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{IO}
\keyword{utilities}
