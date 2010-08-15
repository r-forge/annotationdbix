\name{db1ConverterXXX}
\Rdversion{1.1}
\alias{db1Converter}
\alias{db1ConverterXXX}
\alias{db1ConverterECOLIK12}
\title{Converts .db0 Packages into .db1 Packages}
\description{
Each \command{db1ConverterXXX} method converts a \emph{.db0} SQLite file into its appropriate \emph{.db1} SQLite file.
}
\usage{
db1ConverterECOLIK12(connType, outputDir)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{connType}{
The path and file name or a \strong{SQLiteConnection} object to a .db0 database of interest.
}
  \item{outputDir}{
The path and name of the newly generated \emph{.db1} file. The function automatically adds the suffix \emph{.db1} at the end of the file name.
}
}
\details{
The Bioconductor Project supports many different organism and species with annotation packages. These packages have a .db0 suffix. To check which \emph{.db0} packages are available use the \command{available.chipdbschemas()} function from the \emph{AnnotationDbi} package. To use such annotation packages with the \emph{AnnotationDbiX} package these packages have to be converted into correct \emph{.db1} SQLite databases. Because each \emph{.db0} database differs in the data it stores and there are no concrete rules how the database has to be structured it is not possible to define a generic function that works for all .db0 packages. Thus, for each \emph{.db0} package an aligned \command{db1ConverterXXX()} function has to be written. See the \sQuote{How to Build a \emph{.db1} Database from a \emph{.db0} Database} vignette for more details. These \emph{.db1} packages are needed for the \command{addNewAnnotationFromDb1}() function to add its annotations to the extended \emph{.db} package. The \command{db1ConverterXXX}() function does not change, delete or upgrade any existing data from the \emph{.db0} database. It only adds important tables for the \command{addNewAnnotationFromDb1()} function. Therefore, a \emph{.db1} database can be further used as a \emph{.db0} database.
}
\value{
TRUE, if the function succeeds.
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
	\command{\link{makeDBPackage}()}, \command{\link{addNewAnnotationFromDb1}()}, \command{\link{addNewAnnotation}()} and the \sQuote{\emph{How to Build a .db1 Database from a .db0 Database}} vignette.
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
