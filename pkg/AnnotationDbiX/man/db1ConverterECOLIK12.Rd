\name{db1ConverterECOLIK12}
\Rdversion{1.1}
\alias{db1ConverterECOLIK12}
\title{Convert .db0 packages into .db1 packages}
\description{
Each db1Converter method converts a .db0 database into a .db1 database for which it is designed. The suffix of the db1Converter method is named after the appropriate .db0 database. These .db1 packages are needed for the addNewAnnotationFromDb1() function to add its annotations to the .dbX package. For each .db0 database has its own db1Converter function to create. The db1Converter function must not change, delete or upgrade existing data from the .db0 database. It only add important tables. This ensures that a .db1 database can also be used as a .db0 database.
}
\usage{
db1ConverterXXX(connType, outputDir)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{connType}{
The path and filename or a SQLiteConnection object to the .db0 database of interest.
}
  \item{outputDir}{
The path and name of the new generated .db1 file. The function automatically adds the suffix .db1.
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
\author{N. Auer}
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
structure(function (connType, outputDir) 
standardGeneric("db1ConverterECOLIK12"), generic = structure("db1ConverterECOLIK12", package = "AnnotationDbiX"), package = "AnnotationDbiX", group = list(), valueClass = character(0), signature = c("connType", 
"outputDir"), default = <S4 object of class structure("MethodsList", package = "methods")>, skeleton = function (connType, 
    outputDir) 
stop("invalid call in method dispatch to \"db1ConverterECOLIK12\" (no default method)", 
    domain = NA)(connType, outputDir), class = structure("standardGeneric", package = "methods"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
