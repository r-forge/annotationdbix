\name{db1ConverterXXX}
\Rdversion{1.1}
\alias{db1Converter}
\alias{db1ConverterXXX}
\alias{db1ConverterECOLIK12}
\title{Convert .db0 packages into .db1 packages}
\description{
Each \command{db1ConverterXXX} method converts a .db0 database into a .db1 SQLite database.
}
\usage{
db1ConverterECOLIK12(connType, outputDir)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{connType}{
The path and filename or a SQLiteConnection object to the .db0 database of interest.
}
  \item{outputDir}{
The path and name of the newly generated .db1 file. The function automatically adds the suffix .db1.
}
}
\details{
The Bioconductor Project supports many different organisms and species with annotation packages. These packages have a .db0 suffix. To check which .db0 packages are available use the \command{available.chipdbschemas} function. To use such annotation packages with the AnnotationDbiX package these packages have to be converted into correct .db1 packages. Because each .db0 database differs in the data it stores and there are no concrete rules how the database has to be structured it is not possible to define a generic function that works for all .db0 packages. Thus, for each .db0 package an aligned \command{db1ConverterXXX} function has to be written. See the \sQuote{How to build a .db1 database from a .db0 database} vignette for more details. These .db1 packages are needed for the \command{addNewAnnotationFromDb1} function to add its annotations to the extendend .db package. The \command{db1ConverterXXX} function does not change, delete or upgrade any existing data from the .db0 database. It only adds important tables for the \command{addNewAnnotationFromDb1} function. Therefore, a .db1 database can be further used as a .db0 database.
}
\value{
If the function succeeds it returns TRUE.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{N. Auer \email{norbert@derauer.net}}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
	\command{\link{makeDBPackage}}, \command{\link{addNewAnnotationFromDb1}}, \command{\link{addNewAnnotation}} and the \sQuote{\emph{How to build a .db1 database from a .db0 database}} vignette.
}
\examples{
\dontrun{
db1ConverterECOLIK12 (system.file (package='AnnotationDbiX',
        'data','ecoliK12v2.4.1.sqlite'), 
    outputDir='./ecoliK12v2.4.1')
}}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{IO}
\keyword{utilities}
