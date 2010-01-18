\name{listBimapObj}
\alias{listBimapObj}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{T5
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
listBimapObj(dbfile)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dbfile}{
%%     ~~Describe \code{dbfile} here~~
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
function (dbfile) 
{
    if (!file.exists(dbfile)) 
        stop("Database do not exist!")
    require("RSQLite")
    drv <- dbDriver("SQLite")
    con <- dbConnect(drv, dbname = dbfile)
    on.exit(dbDisconnect(con))
    if (!dbExistsTable(con, "bimap_meta")) {
        cat("There are no bimap objects in database. Use addBimapObj() to add bimap objects.\n")
        return(FALSE)
    }
    sql <- "SELECT * FROM bimap_meta"
    return(dbGetQuery(con, sql))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
