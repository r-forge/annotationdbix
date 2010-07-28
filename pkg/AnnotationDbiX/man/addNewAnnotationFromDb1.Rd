\name{addNewAnnotationFromDb1}
\Rdversion{1.1}
\alias{addNewAnnotationFromDb1}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Add new Annotations from a .db1 database to an extended SQLite annotation database
}
\description{
\command{addNewAnnotationFromDb1} add the data from a converted .db1 SQLite database to an extended annotation package.
}
\usage{
addNewAnnotationFromDb1(x, data, mapTableName, mapDb1TableName, 
    dbSrc)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
The path and file name or a SQLiteConnection object to the extended SQLite database of interest.
}
  \item{data}{
  %where the column on the left contains probe or probeset identifiers and the column on the right contains some sort of widely accepted gene accession
  %one kind of information and that is a list of paired IDs
  
A two column data frame where the column on the left contains probe or probeset identifiers from the type of the table named as \sQuote{mapTableName}. These ids are used to map the new data correctly to the existing annotations in the package. And the column on the right contains some sort of widely accepted gene accession from the .db1 package paired with the ids from the first column. 
}
  \item{mapTableName}{
The name of the table from the annotation package which should be mapped. The table \sQuote{probes} would be a good candidate to map.
}
  \item{mapDb1TableName}{
The name of the table from the .db1 annotation package which should be mapped. The table \sQuote{genes} if present would be a good candidate to map.
}
  \item{dbSrc}{
The path and file name or a SQLiteConnection object to the .db1 package of interest.
}
}
\details{
\command{addNewAnnotationFromDb1} is a little bit analogous to the \command{makeDBPackage} function from the \emph{AnnotationDbi} package. However, instead of a .db0 package it uses a converted .db1 package. See the \sQuote{How to build a .db1 database from a .db0 database} vignette for more details.
}
\value{
If the function succeeds it returns TRUE. 
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
	\command{\link{addNewAnnotation}}, \command{\link{makeDbX}} and \command{db1ConverterXXX}.
}
\examples{
library(AnnotationDbiX)

\dontrun{
feature.seq.list <- read.table(file=system.file(
    package='AnnotationDbiX','data','feature.seq.list'),
    sep="\t",header= FALSE,stringsAsFactors= FALSE)

probe_entrez.list <- read.table(file=system.file(
    package='AnnotationDbiX','data', 'probe_id_entrez_list'),
    sep="\t",header= FALSE,stringsAsFactors= FALSE)
   
makeDbX(feature.seq.list,'Eschericha Coli','Ecoli','ecoliK12CHIP',
    '~/Desktop','1.0.0','Mo-Ferm','ecoliK12 Chip',
    'www.bioconductor.org',author=c('N. Auer','T. Scharl'),
    maintainer='N. Auer <norbert@derauer.net>')

db1ConverterECOLIK12 (system.file (package='AnnotationDbiX','data',
    'ecoliK12v2.4.1.sqlite'),'./ecoliK12v2.4.1')

addNewAnnotationFromDb1(
    x='~/Desktop/ecoliK12CHIP.db/inst/extdata/ecoliK12CHIP.db',
    data=probe_entrez.list,mapT='probes',mapD='genes', 
    dbSrc='ecoliK12v2.4.1.db1')}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{IO}
\keyword{utilities}
