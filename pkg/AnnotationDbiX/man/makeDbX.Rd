\name{makeDbX}
\Rdversion{1.1}
\alias{makeDbX}
\alias{makeDbX-methods}
\title{Generates a extendend annotation database skeleton}
\description{
	In the following sections the generation of extendend SQLite based annotation packages from type .db is described.
}
\usage{
makeDbX(probeList, organism, species, prefix, outputDir, version, 
    chipName, author, maintainer, 
    manufacturer = "Manufacturer not specified", 
    manufacturerUrl = "ManufacturerUrl not specified", 
    tableName = "sequence", 
    colName = "sequence",
    link = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{probeList}{
	A twocolumn list. On the left side are the probe ids or feature names and on the right side are unique identifier ids. For example the nucleotid sequence of the primer. This id must be absolutely unique and must not be \strong{empty}. For example the Entrez id is unique but if the same gene appears more than once in a genome it has a own id for each position in the genome. If the Entrez id is used to create the package only one Entrez id can mapped to one probe id (1:1 mapping except there are other probe ids with the same gene). For example if Blattner ids are added to package it may be happen that some Blattner ids get lost because they map one of the discarded Entrez ids.
}
  \item{organism}{
	The name of the organism e.g. Homo sapiens
}
  \item{species}{
	The name of the species e.g. Human
}
  \item{prefix}{
  	prefix is the first part of the eventual desired package name. (ie. \strong{\dQuote{prefix.db}})

}
  \item{outputDir}{
  	Where you would like the output files to be placed.
}
  \item{version}{
	What is the version number for the desired package.
}
  \item{chipName}{
	What is the name of the chip.
}
  \item{author}{
	List of authors involved in making the package.
}
  \item{maintainer}{
	List of package maintainers with email addresses for contact purposes.
}
  \item{manufacturer}{
	Who made the chip being described.
}
  \item{manufacturerUrl}{
	URL for manufacturers website.
}
  \item{tableName}{
	How the database table should be named. \strong{\dQuote{probe}}, \strong{\dQuote{probe_temp}} and all names including the string \strong{\dQuote{meta}} are not allowed. If no name is specified the default name \strong{\dQuote{sequence}} is used.
}
  \item{colName}{
	How the right column of the new table should be named. This name is important for generating new \link[=Bimap]{Bimap objects}}. The names \strong{\dQuote{_id}} and \strong{\dQuote{probe_id}} are reserved.
  \item{link}{
    A HTML link to a web resource associating to the kind of data which is used. See \code{\link{setIdLink}} for more details.
}
}

\details{
	\code{\link{makeDbX}} generates a annotation package similar to the \code{\link{makeDBPackage}} function from the \emph{AnnotationDbi} package but store the data internally different. One reason for the re-development is the circumstance that the \code{\link{makeDBPackage}} function cannot generate a correct working annotation package for the \emph{E. Coli} K12 strain. Another reason is that the \code{\link{makeDBPackage}} function only can generate an annotation package for a custom microarray if the used organism is supported by the Bioconductor team with a convenient .db0 package. Read the \code{SQLForge} vignette for more detail. \code{\link{makeDbX}} builds only the first two tables. The probes table including the feature names and the second table contains an unique identifier e.g. the nucleotid sequence of the primer. This step is needed to generate an internal id which connect this two tables and future tables together. At this step already no \link[=Bimap]{Bimap objects}} have been generated. Use the \code{\link{addNewAnnotation}} functions to enlarge the annotation package with new annotation data and the \code{\link{addBimapObj}} function to generate \link[=Bimap]{Bimap objects}} in the new annotation package.
}
\value{
	If the function succeed it returns TRUE.
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
	\command{\link{makeDBPackage}}, \command{\link{addNewAnnotationFromDb1}}, \command{\link{addNewAnnotation}}, \command{\link{setIdLink}} and \command{\link{addBimapObj}}.
}
\examples{
\dontrun{
feature.seq.list <- read.table(file=system.file(package='AnnotationDbiX',
    'data','feature.seq.list'),sep="\t",header= FALSE,stringsAsFactors= FALSE)
    
makeDbX(feature.seq.list,'Eschericha Coli','EColi','ecoliK12CHIP','~/Desktop',
     '1.0.0','Mo-Ferm','ecoliK12 Chip','www.bioconductor.org',
     author=c('N. Auer','T. Scharl'),
     maintainer='N. Auer <norbert@derauer.net>')}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{IO}
\keyword{utilities}
