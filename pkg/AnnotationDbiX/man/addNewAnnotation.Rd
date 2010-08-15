\name{addNewAnnotation}
\Rdversion{1.1}
\alias{addNewAnnotation}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Add New Annotations to an Extended .db SQLite Annotation Package.
}
\description{
The \command{addNewAnnotation()} function adds a new table with custom data to an extended \emph{.db} SQLite annotation package.
}
\usage{
addNewAnnotation(x, data, newTableName, data.colNames, mapTableName, 
    tableTypeLength,bimapName)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
The path and file name or a \strong{SQLiteConnection} object to an extended \emph{.db} SQLite database of interest.
}
  \item{data}{
A data frame including the data which should be added to the annotation package, where the column on the left contains identifiers from the same type of the table named as \emph{\sQuote{mapTableName}}. These identifiers are used to map the new data correctly to the existing annotations in the package. The next columns are the identifiers or information which should be added. In the first two columns no NAs or empty entries are allowed. In the next columns optional attributes are allowed. Empty entries are allowed. This data are mostly additional attributes describing the main column in more detail e.g. the \emph{\sQuote{ontology}} column from the Gene Ontology identifiers determine from which kind of ontology the Gene Ontology identifiers are. 
}
  \item{newTableName}{
The name of the new table in the database. The table name must not already exist in the database.
}
  \item{data.colNames}{
A character vector of column names for the new table. For each column of \sQuote{data} except for the first a name must be specified in the same order as the columns in \emph{\sQuote{data}} are listened.
}
  \item{mapTableName}{
The name of the table from the annotation package which should be mapped. The table \emph{\sQuote{probes}} or another unique identifiers would be good candidates to map.
}
	\item{tableTypeLength}{
The maximum expected string length of the entries from the \emph{\sQuote{data}} except for the first column in \emph{\sQuote{data}} in the same order as the columns in \emph{\sQuote{data}} are listened.
}
	\item{bimapName}{
  An optional Bimap object name. If \emph{\sQuote{bimapName}} was set a Bimap object between the \emph{\sQuote{probes}} table and the new table specified in \dQuote{tableName} is generated.
}
}
\details{
It is also possible to map the data to another existing table in the database than to the \emph{\sQuote{probes}} table but it is highly important that these identifiers are unique within the database and have no multi bindings to the probe set like the \emph{Gene Ontology terms} have or the data is related to these identifiers.
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
	\command{\link{addNewAnnotationFromDb1}}, \command{\link{makeDbX}} and \command{db1ConverterXXX}.
}
\examples{
library(AnnotationDbiX)

\dontrun{
feature.seq.list <- read.table(file=system.file(package='AnnotationDbiX',
    'data','feature.seq.list'),sep="\t",header= FALSE,
    stringsAsFactors= FALSE)

feature.blatt.list <- read.table(file=system.file(
    package='AnnotationDbiX','data','blattner_list'),sep="\t",
    header= FALSE,stringsAsFactors= FALSE)
   
makeDbX(feature.seq.list,'Eschericha Coli','Ecoli','ecoliK12CHIP',
    '~/Desktop','1.0.0','Mo-Ferm','ecoliK12 Chip','www.bioconductor.org',
    author=c('N. Auer','T. Scharl'),
    maintainer='N. Auer <norbert@derauer.net>')

# Add other columns from the .gal file to annotation package
addNewAnnotation(
    x='~/Desktop/ecoliK12CHIP.db/inst/extdata/ecoliecoliK12CHIP.db',
    data=feature.blatt.list,newTableName='blattner',
    data.colNames='blattner_id',mapTableName='probes')}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{IO}
\keyword{utilities}
