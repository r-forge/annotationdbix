library(AnnotationDbiX)

probe_entrez.list <- read.table(file=system.file(package='AnnotationDbiX','data','probe_id_entrez_list'),sep=",",header= FALSE,stringsAsFactors= FALSE)
probe_genbank.list <- read.table(file=system.file(package='AnnotationDbiX','data','probe_id_genbank_list'),sep=",",header= FALSE,stringsAsFactors= FALSE)  
feature.seq.list <- read.table(file=system.file(package='AnnotationDbiX','data','feature.seq.list'),sep=",",header= FALSE,stringsAsFactors= FALSE) 

cat('\nErzeuge aus .db0 .db1 Datenbank\n\n')
db1ConverterECOLIK12 (system.file (package='AnnotationDbiX','data','ecoliK12v2.4.1.sqlite'),'./ecoliK12v2.4.1')

cat('\nErzeuge neue .dbX Datenbank mit Sequenzdaten und ProbeIDs für Entrez IDs\n\n')
makeDbX(feature.seq.list,'Eschericha Coli','EColi','ecoliK12CHIPEntrez','~/Desktop','0.1.0','Mo-Ferm','ecoliK12Entrez Chip','www.derauer.net',author=c('Norbert Auer','Test Dummy'),maintainer='Norbert Auer <norbert@derauer.net>')

cat('\nErzeuge neue .dbX Datenbank mit Sequenzdaten und ProbeIDs für Genbnak IDs\n\n')
makeDbX(feature.seq.list,'Eschericha Coli','EColi','ecoliK12CHIPGenbank','~/Desktop','0.1.0','Mo-Ferm','ecoliK12Genbank Chip','www.derauer.net',author=c('Norbert Auer','Test Dummy'),maintainer='Norbert Auer <norbert@derauer.net>')

cat('\nFüge der .dbX Datenbank die Annotationen aus der .db1 hinzu für Entrez IDs\n\n')
addNewAnnotationFromDb1(x='~/Desktop/ecoliK12CHIPEntrez.dbX/inst/extdata/ecoliK12CHIPEntrez.dbX',data=probe_entrez.list,mapT='probes',mapD='genes', dbSrc='ecoliK12v2.4.1.db1')

cat('\nFüge der .dbX Datenbank die Annotationen aus der .db1 hinzu für Genbank IDs\n\n')
addNewAnnotationFromDb1(x='~/Desktop/ecoliK12CHIPGenbank.dbX/inst/extdata/ecoliK12CHIPGenbank.dbX',data=probe_genbank.list,mapT='probes',mapD='genbank_id', dbSrc='ecoliK12v2.4.1.db1')

cat('\nFüge neues Bimap-Objekt SEQUENCE hinzu\n\n')
addBimapObj('~/Desktop/ecoliK12CHIPEntrez.dbX/inst/extdata/ecoliK12CHIPEntrez.dbX','SEQUENCE','probes','sequence')
addBimapObj('~/Desktop/ecoliK12CHIPGenbank.dbX/inst/extdata/ecoliK12CHIPGenbank.dbX','SEQUENCE','probes','sequence')

install.packages(pkgs='~/Desktop/ecoliK12CHIPEntrez.dbX/',repos=NULL)
install.packages(pkgs='~/Desktop/ecoliK12CHIPGenbank.dbX/',repos=NULL)

library(ecoliK12CHIPGenbank.dbX)
library(ecoliK12CHIPEntrez.dbX)

probeIDs <- as.data.frame(Lkeys(ecoliK12CHIPEntrezENTREZID),stringsAsFactors=FALSE)
colnames(probeIDs) <- "probe_id"
mergedEntrez <- merge(probeIDs,toTable(ecoliK12CHIPEntrezENTREZID),by.x='probe_id',by.y='probe_id',all.x=TRUE)
mergedEntrez <- unique(rbind(mergedEntrez,toTable(ecoliK12CHIPGenbankENTREZID)))
mergedEntrez <- mergedEntrez[!is.na(mergedEntrez[[2]]),]

# Dublikate EntrezIds zu den ProbeIds
multiEntrez <- mergedEntrez[mergedEntrez[[1]] %in% mergedEntrez[duplicated(mergedEntrez[[1]]),1],]

mergedGenbank <- merge(probeIDs,toTable(ecoliK12CHIPEntrezACCNUM),by.x='probe_id',by.y='probe_id',all.x=TRUE)
mergedGenbank <- merge(mergedGenbank,toTable(ecoliK12CHIPGenbankACCNUM),by.x='probe_id',by.y='probe_id',all.x=TRUE)

# Dublikate GenbankIds zu den ProbeIds
multiGenbank <- mergedGenbank[mergedGenbank[[1]] %in% mergedGenbank[duplicated(mergedGenbank[[1]]),1],]

# Neues Annotationspaket mit mehr IDs
cat('\nErzeuge neue .dbX Datenbank mit Sequenzdaten und ProbeIDs für gemergetes Annotationspaket\n\n')
makeDbX(feature.seq.list,'Eschericha Coli','EColi','ecoliK12CHIP','~/Desktop','0.1.0','Mo-Ferm','ecoliK12 Chip','www.derauer.net',author=c('Norbert Auer','Test Dummy'),maintainer='Norbert Auer <norbert@derauer.net>')

cat('\nFüge der .dbX Datenbank die Annotationen aus der .db1 hinzu für Entrez IDs\n\n')
addNewAnnotationFromDb1(x='~/Desktop/ecoliK12CHIP.dbX/inst/extdata/ecoliK12CHIP.dbX',data=mergedEntrez,mapT='probes',mapD='genes', dbSrc='ecoliK12v2.4.1.db1')

addBimapObj('~/Desktop/ecoliK12CHIP.dbX/inst/extdata/ecoliK12CHIP.dbX','SEQUENCE','probes','sequence')

install.packages(pkgs='~/Desktop/ecoliK12CHIP.dbX/',repos=NULL)







if(FALSE)
{


addNewAnnotationFromDb1(x='~/Desktop/ecoliK12CHIP.dbX/inst/extdata/ecoliK12CHIP.dbX',data=probe_genbank.list,mapT='probes',mapD='genbank_id', dbSrc='ecoliK12v2.3.5.db1')

cat('\nErzeuge dummy Daten\n\n')	
dummy_data <- data.frame(sample(feature.seq.list[[1]],2000),rnorm(2000))

cat('\nFüge externe dummy Daten dem .dbX hinzu\n\n')
addNewAnnotation(x='~/Desktop/ecoliK12CHIP.dbX/inst/extdata/ecoliK12CHIP.dbX', data=dummy_data,newTableName='dummy',data.colNames='dummy_id',mapTableName='probes')

cat('\nFüge neues Bimap-Objekt Dummy Daten hinzu\n\n')
addBimapObj('~/Desktop/ecoliK12CHIP.dbX/inst/extdata/ecoliK12CHIP.dbX','DUMMY','probes','dummy')

cat('\nLasse mir die CreateStatements anzeigen um vorhandene Tablenamen herauszufinden\n\n')
print(.dbSchema(x='~/Desktop/ecoliK12CHIP.dbX/inst/extdata/ecoliK12CHIP.dbX'))

cat('\nFüge ein paar HTML-Links zur Datenbank, die dann für die HTML-Links der IDs verwendet werden.\nDort wo die IDs normal im Link stehen wird als Platzhalter $ID verwendet\n\n')
setIdLink('~/Desktop/ecoliK12CHIP.dbX/inst/extdata/ecoliK12CHIP.dbX', "kegg", "http://www.genome.jp/dbget-bin/www_bget?pathway:eco$ID")
setIdLink('~/Desktop/ecoliK12CHIP.dbX/inst/extdata/ecoliK12CHIP.dbX', "genbank_id", "http://www.ncbi.nlm.nih.gov/entrez/query.fcgi?db=Nucleotide&cmd=search&term=$ID")
setIdLink('~/Desktop/ecoliK12CHIP.dbX/inst/extdata/ecoliK12CHIP.dbX', "genes", "http://www.ncbi.nlm.nih.gov/entrez/query.fcgi?db=gene&cmd=Retrieve&dopt=Graphics&list_uids=$ID")
setIdLink('~/Desktop/ecoliK12CHIP.dbX/inst/extdata/ecoliK12CHIP.dbX', "refseq", "http://www.ncbi.nlm.nih.gov/entrez/query.fcgi?db=Nucleotide&cmd=search&term=$ID")
setIdLink('~/Desktop/ecoliK12CHIP.dbX/inst/extdata/ecoliK12CHIP.dbX', "ec", "http://www.genome.jp/dbget-bin/www_bget?ec:$ID")

cat('\nInstalliere neu erstellte .dbX Datenbank\n\n')	
install.packages(pkgs='~/Desktop/ecoliK12CHIP.dbX/',repos=NULL)

cat('\nLade neu erstellte .dbX Datenbank und führe einige Funktionen aus\n\n')	
library(ecoliK12CHIP.dbX)

cat('\nEin paar Versuche...\n\n')
ecoliK12CHIPMAPCOUNTS

print(toTable(ecoliK12CHIPSEQUENCE)[1:10,])
print(toTable(ecoliK12CHIPDUMMY))

cat('\nGeneriere Dummy external data\n\n')
rprobe_ids <- feature.seq.list[sample(nrow(feature.seq.list),210),]
extdata <- data.frame(probe_id=rprobe_ids[[1]],extdata1=rnorm(210),extdata2=rnorm(210),seq_length=nchar(rprobe_ids[[2]]))

cat('\nGeneriere eine tolle HTML\n\n')
annotationPkgToHTML(ecoliK12CHIP_dbconn(),'Testing annotationPkgToHTML function','~/Desktop/test',"probes",c("go_id","ec","genes","kegg","genbank_id","sequence","dummy"),rprobe_ids[[1]],extdata=extdata,colOrder=c("sequence","seq_length","probes","genes","genbank_id","kegg","ec","go_id","extdata1","extdata2","dummy"))

}



