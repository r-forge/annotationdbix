\name{BimapObj functions}
\Rdversion{1.1}
\alias{addBimapObj}
\alias{deleteBimapObj}
\alias{listBimapObj}
\alias{Bimap}
\title{Handling Bimap objects with extended .db SQLite databases}
\description{
	In the following sections is described how Bimap objects can be handled in extended SQLite databases created by the \command{\link{makeDbX}()} function.
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
		The path and file name or a \strong{SQLiteConnection} object to an extended \emph{.db} SQLite database of interest. In the case of \command{deleteBimapObj()} an \strong{AnnDbBimap} object is also allowed.
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
		A named character string naming the column of the left respectively right SQLite table which should be used as a tag. An alias name for tag names can be used which is shown as column name in functions like \command{toTable()} instead of the column name of the SQLite table. Use a named character string to set the alias e.g.
		
		\preformatted{tagname1=c(Evid="evidence")}
	}	
	\item{filter1,filter2}{
		A string describing a SQL constraint which is typically used in a SQL \dQuote{WHERE} Clause. Column names should be enclosed in curly braces. These marked column names are internally extended with table aliases to avoid column name conflicts between the two tables. Therefore, it is important to use only column names from the left table in the \emph{\sQuote{filter1}} argument and vice versa to use only column names from the right table for the \emph{\sQuote{filter2}} argument.
		
		\preformatted{## Example string
		\{colName\} = "cc"}
		
	}
	\item{revmap}{
		There are two possibilities to generate reverse maps of Bimap objects. Firstly you add a new Bimap and exchange the two tables or you set revmap equal TRUE. If revmap is TRUE it generates the Bimap object as a reverse map equivalent to the \command{revmap()} function from the \emph{AnnotationDbi} package. Beware these two methods are not the same. The first method generates a real reverse map whereas the second method only sets the direction slot of the AnnDbBimap object * -1. Functions like the \command{toTable()} function from the \emph{AnnotationDbi} package ignore this slot and the results from the reverse map and the normal map are the same. Other functions like \command{ls()} work also correct with revmaps.
	}
}
\details{
	The way how \link[=Bimap]{Bimap objects} are generated and saved in the \emph{AnnotionDbi} package is different to the way followed in the \emph{AnnotationDbiX package}. In the \emph{AnnotationDbi} package \link[=Bimap]{Bimap objects} are hard coded in the \code{createAnnObjs.XXX.R} files where the \code{XXX} stands for a special database schema which was used for the organism. In the \emph{AnnotationDbiX} package a more generic approach is used. Instead of hard coded files the information about the Bimaps are stored into the database itself. Thus, this package works with all organism and other kind of data without any specialised code files. When the package is loaded at runtime the \link[=Bimap]{Bimap objects} are generated dynamically with the Bimap information stored in the SQLite database. The functions described here are used to handle these information with the database. The \command{addBimapObj()} function add new Bimaps to the extended \emph{.db} SQLite database. The \command{deleteBimapObj()} function deletes a Bimap from the extended \emph{.db} SQLite database. The \code{listBimapObj()} function lists all available Bimaps from the extended \emph{.db} SQLite database.
}
\value{
	TRUE, if the function succeeds. The \command{listBimapObj()} function returns a list of class \emph{data.frame} listing all stored \link[=Bimap]{Bimap objects} which are stored into the extended \emph{.db} SQLite database.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{N. Auer \email{norbert@derauer.net}}
\note{
If a Bimap object is added or deleted, the changes only will take effect after the library is reloaded.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
	\command{Bimap-direction()}, \command{Bimap-keys()}, \command{Bimap-toTable()}, \command{BimapFormatting()}, \command{AnnDbBimap-envirAPI} from the \emph{AnnotationDbi} package and \command{\link{makeDbX}()} from the \emph{AnnotationDbiX} package.
}
\examples{
\dontrun{
library(ecoliK12.db)

## add new Bimap objects
addBimapObj(ecoliK12CHIP_dbconn(),"GO_CCMF","probes","go_id",
    tagname2=c("evidence",Ontology="ontology"),
    filter2="{ontology} = 'cc' OR {ontology} = 'mf'")

addBimapObj(ecoliK12CHIP_dbconn(),"ALIAS2GO","gene_synonyms","go_id")

## the same as revmap(ecoliK12CHIPALIAS2GO,objName="GO2ALIAS")
addBimapObj(ecoliK12CHIP_dbconn(),"GO2ALIAS","gene_synonyms","go_id",
    revmap=TRUE)

## add a real revers map from the ecoliK12CHIPALIAS2GO Bimap
addBimapObj(ecoliK12CHIP_dbconn(),"GO2ALIASREAL","go_id","gene_synonyms")		

## reload the the annotation package
detach(package:ecoliK12.db,unload=TRUE)
library(ecoliK12.db)

# shows all Bimaps stored in the extended .db database
listBimapObj(ecoliK12CHIP_dbconn())

toTable(ecoliK12CHIPGO2ALIAS)[1:10,]

# the revers map is sorted as the normal map
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
