\name{BimapObj functions}
\Rdversion{1.1}
\alias{addBimapObj}
\alias{deleteBimapObj}
\alias{listBimapObj}
\alias{Bimap}
\title{Handling Bimap objects in .dbX databases}
\description{
	In the following sections the handling of Bimap objects in .dbX databases is described.
}
\usage{
addBimapObj(x, name, table1, table2, comment = "Added by addBimapObj()", tagname1 = "", tagname2 = "", filter1 = "", filter2 = "", revmap = FALSE)

deleteBimapObj(x, name)

listBimapObj(x)
}

\arguments{
	\item{x}{
		The path and filename or a SQLiteConnection object to the .dbX database of interest. In the case of deleteBimapObj() an AnnDbBimap object is also allowed.
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
	\item{tagname1,tagname2}{
		A character vector naming the columns of the left respectively right SQLite table which should used as tag. If more than one column is used as tag a warning occurs using this Bimap with the toTable() function. This warning can be safely ignored. An alias name for tagnames can be used which is shown as column name in functions like toTable() instead of the column name of the SQLite table. Use named vectors to set the alias e.g.
		
		\code{tagname1=c(Evid="evidence",Ont="ontology")}
	}	
	
		
	
	\item{filter1,filter2}{
		A string describing a SQL constraint which is typically used in SQL WHERE Clause. Column names should be enclosed in curly braces. These marked column names are internally extended with table aliases to avoid column name conflicts between the two tables. Therefore it is important to use only column names from the left table in the filter1 argument and vice versa for the filter2 argument.
		
		## Example string
		\code{
		\{colName\} = "cc"}
		
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
\author{N. Auer}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
	Bimap-direction, Bimap-keys, Bimap-toTable, BimapFormatting and AnnDbBimap-envirAPI from the AnnotationDbi package
}
\examples{
\dontrun{
## add new Bimaps
addBimapObj(ecoliK12CHIP_dbconn(),"GO_CCMF","probes","go_id",tagname2=c("evidence",Ontology="ontology"),filter2="{ontology} = 'cc' OR {ontology} = 'mf'")
addBimapObj(ecoliK12CHIP_dbconn(),"ALIAS2GO","gene_synonyms","go_id")

## the same like revmap(ecoliK12CHIPALIAS2GO,objName="GO2ALIAS")
addBimapObj(ecoliK12CHIP_dbconn(),"GO2ALIAS","gene_synonyms","go_id",revmap=TRUE)

## add a real revers map from ecoliK12CHIPALIAS2GO Bimap
addBimapObj(ecoliK12CHIP_dbconn(),"GO2ALIASREAL","go_id","gene_synonyms")		

## Start a new session
# Shows all Bimaps stored in the .dbX database
listBimapObj(ecoliK12CHIP_dbconn())

toTable(ecoliK12CHIPGO2ALIAS)[1:10,]

# The revers map is sorted like the normal map
toTable(ecoliK12CHIPALIAS2GO)[1:10,]

toTable(ecoliK12CHIPGO2ALIASREAL)[1:10,]
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
