\name{makeDbX}
\Rdversion{1.1}
\alias{makeDbX}
\alias{makeDbX-methods}
\title{How to create an extended annotation database skeleton}
\description{
	In the following sections is described how an extended SQLite-based annotation package from type \emph{.db} is created.
}
\usage{
makeDbX(probeList, organism, species, prefix, outputDir, version, 
    chipName, author, maintainer, 
    manufacturer = "Manufacturer not specified", 
    manufacturerUrl = "ManufacturerUrl not specified", 
    tableName = "sequence", 
    colName = "sequence",
    bimapName,
    link = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{probeList}{
	A two-column list. On the left side are the probe ids or feature names and on the right side are unique identifier ids. For example the nucleotide sequence of the primer. This identifier must be absolutely unique and must not be empty. For example the Entrez Gene identifier is unique but if the same gene appears more than once in a genome it has an own identifier number for each position in the genome. If the Entrez Gene identifier is used to create the package only one Entrez Gene identifier can be mapped to one probe id (1:1 mapping unless there are other probe identifiers with the same gene). For example if Blattner identifiers are added to the package it may happen that some Blattner identifiers get lost because they map one of the discarded Entrez Gene identifiers.
}
  \item{organism}{
	The name of the organism e.g. Homo sapiens
}
  \item{species}{
	The name of the species e.g. Human
}
  \item{prefix}{
  	Prefix is the first part of the eventual desired package name. (e.g. \emph{\sQuote{prefix.db}})
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
	How the database table should be named. \emph{\sQuote{probe}}, \emph{\sQuote{probe_temp}} and all names including the string \emph{\sQuote{meta}} are not allowed. If no name is specified the default name \emph{\sQuote{sequence}} is used.
}
  \item{colName}{
	How the right column of the new table should be named. This name is important for generating new \link[=Bimap]{Bimap objects}. The names \emph{\sQuote{_id}} and \emph{\sQuote{probe_id}} are reserved.
}
  \item{bimapName}{
  A optional Bimap object name. If \emph{\sQuote{bimapName}} was set a Bimap object between the \emph{\sQuote{probes}} table and the new table specified in \emph{\sQuote{tableName}} is created.
}
  \item{link}{
    A HTML link to a web resource associated to the kind of identifier which is used. See \command{\link{setIdLink}()} for more details.
}
}

\details{
	\command{\link{makeDbX}()} generates an annotation package similar to the \command{\link{makeDBPackage}()} function from the \emph{AnnotationDbi} package but stores the data internally different. A difference is that the \command{\link{makeDBPackage}()} function only can generate an annotation package for a custom microarray if the used organism is supported by the Bioconductor team with a convenient .db0 package. Read the \command{SQLForge} vignette for more detail. The \command{\link{makeDbX}()} function creates the package skeleton with the first two tables. The probes table includes the feature names and the second table contains an unique identifier e.g. the nucleotide sequence of the primer. This step is needed to generate the internal identifier \sQuote{\command{_id}} which connects these two tables and further tables together. At this step no \link[=Bimap]{Bimap object} has been created, yet. Use the \command{\link{addNewAnnotation}()} function to enlarge the annotation package with new annotation data and the \command{\link{addBimapObj}()} function to generate \link[=Bimap]{Bimap objects} in the new annotation package.
}
\value{
	TRUE, if the function succeeds.
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
	\command{\link{makeDBPackage}()}, \command{\link{addNewAnnotationFromDb1}()}, \command{\link{addNewAnnotation}()}, \command{\link{setIdLink}()} and \command{\link{addBimapObj}()}.
}
\examples{
\dontrun{
feature.seq.list <- read.table(file=system.file(package='AnnotationDbiX',
    'data','feature.seq.list'),sep="\t",header= FALSE,
    stringsAsFactors= FALSE)
    
makeDbX(feature.seq.list,'Eschericha Coli','Ecoli','ecoliK12CHIP',
    '~/Desktop','1.0.0','Mo-Ferm','ecoliK12 Chip','www.bioconductor.org',
    author=c('N. Auer','T. Scharl'),
    maintainer='N. Auer <norbert@derauer.net>')}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{IO}
\keyword{utilities}
