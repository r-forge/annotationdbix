\name{addBimapObj}
\alias{addBimapObj}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{T1
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
addBimapObj(dbfile, name, table1, table2, comment = "Added by addBimapObj()", WHERE = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dbfile}{
%%     ~~Describe \code{dbfile} here~~
}
  \item{name}{
%%     ~~Describe \code{name} here~~
}
  \item{table1}{
%%     ~~Describe \code{table1} here~~
}
  \item{table2}{
%%     ~~Describe \code{table2} here~~
}
  \item{comment}{
%%     ~~Describe \code{comment} here~~
}
  \item{WHERE}{
%%     ~~Describe \code{WHERE} here~~
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
function (dbfile, name, table1, table2, comment = "Added by addBimapObj()", 
    WHERE = "") 
{
    if (!file.exists(dbfile)) 
        stop("Database do not exist!")
    require("RSQLite")
    drv <- dbDriver("SQLite")
    con <- dbConnect(drv, dbname = dbfile)
    on.exit(dbDisconnect(con))
    if (!dbExistsTable(con, table1)) 
        stop("Table1 do not exist!")
    if (!dbExistsTable(con, table2)) 
        stop("Table2 do not exist!")
    sql <- "CREATE TABLE IF NOT EXISTS bimap_meta (name TEXT PRIMARY KEY,table1 TEXT NOT NULL,table2 TEXT NOT NULL,comment TEXT,sql TEXT)"
    dbGetQuery(con, sql)
    sql <- paste("SELECT COUNT(*) FROM bimap_meta WHERE name='", 
        name, "'", sep = "")
    if (dbGetQuery(con, sql) > 0) {
        cat("Bimap object '", name, "' already exist. Use deleteBimapObj() to delete bimap object.\n", 
            sep = "")
        return(FALSE)
    }
    sql <- paste("INSERT INTO bimap_meta VALUES ('", name, "','", 
        table1, "','", table2, "','", comment, "','", WHERE, 
        "')", sep = "")
    dbGetQuery(con, sql)
    cat("Bimap Object '", name, "' added.\nDetach the library now and load it new to update existing bimap objects.\n", 
        sep = "")
    return(TRUE)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
