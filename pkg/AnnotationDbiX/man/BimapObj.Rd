\name{BimapObj functions}
\Rdversion{1.1}
\alias{addBimapObj}
\alias{deleteBimapObj}
\alias{listBimapObj}
\title{Handling Bimap objects in .dbX databases}
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
addBimapObj(x, name, table1, table2, comment = "Added by addBimapObj()", tagname1 = "", tagname2 = "", filter1 = "", filter2 = "", revmap = FALSE)

deleteBimapObj(x, name)

listBimapObj(x)
}

\arguments{
	\item{x}{
		The path and filename or a SQLiteConnection object to the .dbX database of interest. 
	}
	\item{name}{
		Name of the Bimap object.
	}
	\item{table1}{
		SQLite table name of the left side of the Bimap.
	}
	\item{table2}{
		SQLite table name of the rigth side of the Bimap.
	}
	\item{comment}{
		Comments for the Bimap object.
	}
	\item{tagname1}{
		A character vector naming the columns of the left SQLite table which should used as tag. If more than one column is used as tag a warning occurs using this Bimap with the toTable() function. This warning can be safely ignored.
	}
	\item{tagname2}{
		A character vector naming the columns of the rigth SQLite table which should used as tag. If more than one column is used as tag a warning occurs using this Bimap with the toTable() function. This warning can be safely ignored.
	}
	\item{filter1,filter2}{
		A string describing a SQL constraint which is typically used in SQL WHERE Clause. Column names should be enclosed in curly braces. These marked column names are internally extended with table aliases to avoid column name conflicts between the two tables. Therefore it is important to use only column names from the left table in the filter1 parameter and vice versa for the filter2 parameter.
		\code{
			## Example string
			\{colName\} = "cc"
		}
	}
  
	\item{revmap}{
		There are two possibilities to generate reverse maps of Bimap objects. Firstly you add a new Bimap and exchange the two tables or you set revmap TRUE. If revmap is TRUE it generates the Bimap object as a reverse map equivalent to the revmap() function from the AnnotationDbi package. Attention these two methods are not the same. The first methode generates a really reverse map whereas the second method only set the direction slot of the AnnDbBimap object * -1. Functions like the toTable() function from the AnnotationDbi package ignore this slot and the results from the reverse map and the normal map are the same.
	}
}
\details{
	If you add or delete a new Bimap object changes will take effect only after the session is restarted.
}

\references{
%% ~put references to the literature/web site here ~
}
\author{Norbert Auer}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
	\dontrun{
	## add a new Bimap
	addBimapObj(ecoliK12CHIP_dbconn(),'SEQUENCE','probes','sequence')
	## the same like revmap(ecoliK12CHIPSEQUENCE,objName="SEQUENCE2PROBE")
	addBimapObj(ecoliK12CHIP_dbconn(),'SEQUENCE2PROBE','probes','sequence',revmap=TRUE)
	## add a real revers map from ecoliK12CHIPSEQUENCE Bimap
	addBimapObj(ecoliK12CHIP_dbconn(),'SEQUENCE2PROBEREAL','sequence','probes')
		
	## Start a new session
	toTable(ecoliK12CHIPSEQUENCE)[1:10,]
	toTable(ecoliK12CHIPSEQUENCE2PROBE)[1:10,]
	toTable(ecoliK12CHIPSEQUENCE2PROBEREAL)[1:10,]
	}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
