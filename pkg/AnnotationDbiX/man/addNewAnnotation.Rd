\name{addNewAnnotation}
\Rdversion{1.1}
\alias{addNewAnnotation}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Add new Annotations to an extendend SQLite annotation package.
}
\description{
\command{addNewAnnotation} add a new table with custom data to an extendend SQLite annotation package.
}
\usage{
addNewAnnotation(x, data, newTableName, data.colNames, mapTableName, 
    tableTypeLength)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
The path and filename or a SQLiteConnection object to the extendent SQLite database of interest.
}
  \item{data}{
A dataframe including the data which should be added to the annotation package. Where the column on the left contains mainly probe or probeset identifiers from the type of the table named as \sQuote{mapTableName}. These ids are used to map the new data correct to the existing annotations in the package. The next column is the id or information which should be added. In the first two columns no NAs or empty entrys are allowed. In the next columns are optional additional information allowed. Empty entrys are allowed. This data are mostly additional attributes describing the main column in more detail e.g. the \sQuote{ontology} column from the GO ids determine from which kind of ontology the go id is. 
}
  \item{newTableName}{
The name of the new table in the database. The table name must not already exist in the database.
}
  \item{data.colNames}{
A character vector of column names for the new table. For each column of \sQuote{data} except for the first a name must be specified in the same order like the columns in \sQuote{data} are ordered.
}
  \item{mapTableName}{
The name of the table from the annotation package which should be mapped. The table \sQuote{probes} would be a good candidate to map.
}
	\item{tableTypeLength}{
The maximum expected string lenght of the entrys from the \sQuote{data} except for the first column in \sQuote{data} in the same order like the columns in \sQuote{data} are ordered.
}
}
\details{
It is also possible to map the data to another existing table in the database than to the \sQuote{probes} table but it is higly important that these identifiers are unique within the database and have no multibindings to the probeset like \sQuote{GO terms} have or the data is related to these identifiers.
}
\value{
If the function succeed it returns TRUE. 
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
    'data','feature.seq.list'),sep="\t",header= FALSE,stringsAsFactors= FALSE)

feature.blatt.list <- read.table(file=system.file(package='AnnotationDbiX',
    'data','blattner_list'),sep="\t",header= FALSE,stringsAsFactors= FALSE)
   
makeDbX(feature.seq.list,'Eschericha Coli','EColi','ecoliK12CHIP','~/Desktop',
     '1.0.0','Mo-Ferm','ecoliK12 Chip','www.bioconductor.org',
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
