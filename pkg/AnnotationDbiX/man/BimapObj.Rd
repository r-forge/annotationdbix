\name{BimapObj functions}
\Rdversion{1.1}
\alias{addBimapObj}
\alias{deleteBimapObj}
\alias{listBimapObj}
\alias{Bimap}
\title{Handling Bimap objects in .dbX databases}
\description{
	In the following sections the handling of Bimap objects in extended SQLite databases created by the \code{\link{makeDbX}} is described.
}
\usage{
addBimapObj(x, name, table1, table2, 
    comment = "Added by addBimapObj()", tagname1 = "", 
    tagname2 = "", filter1 = "", filter2 = "", revmap = FALSE)

deleteBimapObj(x, name)

listBimapObj(x)
}

\arguments{
	\item{x}{
		The path and file name or a SQLiteConnection object to the extended SQLite database of interest. In the case of deleteBimapObj() an AnnDbBimap object is also allowed.
	}
	\item{name}{
		Name of the Bimap object.
	}
	\item{table1}{
		SQLite table name of the left side of the Bimap.
	}
	\item{table2}{
		SQLite table name of the right side of the Bimap.
	}
	\item{comment}{
		Comments for the Bimap object.
	}
	\item{tagname1,tagname2}{
		A character vector naming the columns of the left respectively right SQLite table which should be used as tag. If more than one column is used as tag a warning occurs using this Bimap with the toTable() function. This warning can be safely ignored. An alias name for tag names can be used which is shown as column name in functions like toTable() instead of the column name of the SQLite table. Use named vectors to set the alias e.g.
		
		\preformatted{tagname1=c(Evid="evidence",Ont="ontology")}
	}	
	\item{filter1,filter2}{
		A string describing a SQL constraint which is typically used in SQL WHERE Clause. Column names should be enclosed in curly braces. These marked column names are internally extended with table aliases to avoid column name conflicts between the two tables. Therefore it is important to use only column names from the left table in the filter1 argument and vice versa for the filter2 argument.
		
		\preformatted{## Example string
		\{colName\} = "cc"}
		
	}
	\item{revmap}{
		There are two possibilities to generate reverse maps of Bimap objects. Firstly you add a new Bimap and exchange the two tables or you set revmap equal TRUE. If revmap is TRUE it generates the Bimap object as a reverse map equivalent to the revmap() function from the AnnotationDbi package. Attention these two methods are not the same. The first method generates a real reverse map whereas the second method only sets the direction slot of the AnnDbBimap object * -1. Functions like the toTable() function from the AnnotationDbi package ignore this slot and the results from the reverse map and the normal map are the same. Other functions like ls() work also correct with with revmaps.
	}
}
\details{
	The way how \link[=Bimap]{Bimap objects} are generated and saved in the AnnotionDbi package is different to the way followed in the AnnotationDbiX package. In the AnnotationDbi package \link[=Bimap]{Bimap objects} are hard coded in the \code{createAnnObjs.XXX.R} files at which the \code{XXX} stands for the special database schema which was used for the organism. In the AnnotationDbiX package a more generic approach is used. Instead of hard coded files the information about the Bimaps are stored into the database itself. Thus, this package works with all organism and other kind of data without any specialised code files. When the package is loaded at runtime the \link[=Bimap]{Bimap objects} are generated dynamically with the Bimap information stored in the SQLite database. The functions described here are used to handle these information with the database. \code{addBimapObj} add new Bimaps to the SQLite database. \code{deleteBimapObj} deletes a Bimap from the SQLite database. \code{listBimapObj} lists all available Bimaps from the SQLite database.
}
\value{
	If the function succeeds it returns TRUE. \code{listBimapObj} returns a list of class \code{data.frame} listing all stored \link[=Bimap]{Bimap objects} stored into the SQLite database.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{N. Auer \email{norbert@derauer.net}}
\note{
If you add or delete a new Bimap object changes will take effect only after the session is restarted.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
	\command{Bimap-direction}, \command{Bimap-keys}, \command{Bimap-toTable}, \command{BimapFormatting}, \command{AnnDbBimap-envirAPI} from the AnnotationDbi package and \command{\link{makeDbX}} from the AnnotationDbiX package.
}
\examples{
\dontrun{
## add new Bimaps
addBimapObj(ecoliK12CHIP_dbconn(),"GO_CCMF","probes","go_id",
    tagname2=c("evidence",Ontology="ontology"),
    filter2="{ontology} = 'cc' OR {ontology} = 'mf'")

addBimapObj(ecoliK12CHIP_dbconn(),"ALIAS2GO","gene_synonyms","go_id")

## the same like revmap(ecoliK12CHIPALIAS2GO,objName="GO2ALIAS")
addBimapObj(ecoliK12CHIP_dbconn(),"GO2ALIAS","gene_synonyms","go_id",
    revmap=TRUE)

## add a real revers map from ecoliK12CHIPALIAS2GO Bimap
addBimapObj(ecoliK12CHIP_dbconn(),"GO2ALIASREAL","go_id","gene_synonyms")		

## Start a new session
# Shows all Bimaps stored in the .dbX database
listBimapObj(ecoliK12CHIP_dbconn())

toTable(ecoliK12CHIPGO2ALIAS)[1:10,]

# The revers map is sorted like the normal map
toTable(ecoliK12CHIPALIAS2GO)[1:10,]

toTable(ecoliK12CHIPGO2ALIASREAL)[1:10,]

ls(ecoliK12CHIPGO2ALIAS)[1:10]
ls(ecoliK12CHIPALIAS2GO)[1:10]
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{IO}
\keyword{utilities}
