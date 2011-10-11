library(AnnotationDbiX)

probe_entrez.list <- read.table(file=system.file(package='AnnotationDbiX','data','probe_id_entrez_list'),sep="\t",header= FALSE,stringsAsFactors= FALSE)
probe_genbank.list <- read.table(file=system.file(package='AnnotationDbiX','data','probe_id_genbank_list'),sep="\t",header= FALSE,stringsAsFactors= FALSE)  
feature.seq.list <- read.table(file=system.file(package='AnnotationDbiX','data','feature.seq.list'),sep="\t",header= FALSE,stringsAsFactors= FALSE) 

cat('\nErzeuge aus .db0 .db1 Datenbank\n\n')
db1ConverterECOLIK12 (system.file (package='AnnotationDbiX','data','ecoliK12v2.4.1.sqlite'),'~/Arbeitsfläche/ecoliK12v2.4.1')

cat('\nErzeuge neue .db Datenbank mit Sequenzdaten und ProbeIDs für Entrez IDs\n\n')
makeDbX(feature.seq.list,'Eschericha Coli','EColi','ecoliK12CHIPEntrez','~/Arbeitsfläche','0.1.0','Mo-Ferm','ecoliK12Entrez Chip','www.derauer.net',author=c('Norbert Auer','Test Dummy'),maintainer='Norbert Auer <norbert@derauer.net>')

cat('\nErzeuge neue .db Datenbank mit Sequenzdaten und ProbeIDs für Genbnak IDs\n\n')
makeDbX(feature.seq.list,'Eschericha Coli','EColi','ecoliK12CHIPGenbank','~/Arbeitsfläche','0.1.0','Mo-Ferm','ecoliK12Genbank Chip','www.derauer.net',author=c('Norbert Auer','Test Dummy'),maintainer='Norbert Auer <norbert@derauer.net>')

cat('\nFüge der .db Datenbank die Annotationen aus der .db1 hinzu für Entrez IDs\n\n')
addNewAnnotationFromDb1(x='~/Arbeitsfläche/ecoliK12CHIPEntrez.db/inst/extdata/ecoliK12CHIPEntrez.db',data=probe_entrez.list,mapT='probes',mapD='genes', dbSrc='~/Arbeitsfläche/ecoliK12v2.4.1.db1')

cat('\nFüge der .db Datenbank die Annotationen aus der .db1 hinzu für Genbank IDs\n\n')
addNewAnnotationFromDb1(x='~/Arbeitsfläche/ecoliK12CHIPGenbank.db/inst/extdata/ecoliK12CHIPGenbank.db',data=probe_genbank.list,mapT='probes',mapD='genbank_id', dbSrc='~/Arbeitsfläche/ecoliK12v2.4.1.db1')

cat('\nFüge neues Bimap-Objekt SEQUENCE hinzu\n\n')
addBimapObj('~/Arbeitsfläche/ecoliK12CHIPEntrez.db/inst/extdata/ecoliK12CHIPEntrez.db','SEQUENCE','probes','sequence')
addBimapObj('~/Arbeitsfläche/ecoliK12CHIPGenbank.db/inst/extdata/ecoliK12CHIPGenbank.db','SEQUENCE','probes','sequence')

install.packages(pkgs='~/Arbeitsfläche/ecoliK12CHIPEntrez.db/',repos=NULL)
install.packages(pkgs='~/Arbeitsfläche/ecoliK12CHIPGenbank.db/',repos=NULL)

library(ecoliK12CHIPGenbank.db)
library(ecoliK12CHIPEntrez.db)

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
cat('\nErzeuge neue .db Datenbank mit Sequenzdaten und ProbeIDs für gemergetes Annotationspaket\n\n')
makeDbX(feature.seq.list,'Eschericha Coli','EColi','ecoliK12CHIP','~/Arbeitsfläche','0.1.0','Mo-Ferm','ecoliK12 Chip','www.derauer.net',author=c('Norbert Auer','Test Dummy'),maintainer='Norbert Auer <norbert@derauer.net>')

cat('\nFüge der .db Datenbank die Annotationen aus der .db1 hinzu für Entrez IDs\n\n')
addNewAnnotationFromDb1(x='~/Arbeitsfläche/ecoliK12CHIP.db/inst/extdata/ecoliK12CHIP.db',data=mergedEntrez,mapT='probes',mapD='genes', dbSrc='~/Arbeitsfläche/ecoliK12v2.4.1.db1')

addBimapObj('~/Arbeitsfläche/ecoliK12CHIP.db/inst/extdata/ecoliK12CHIP.db','SEQUENCE','probes','sequence')

install.packages(pkgs='~/Arbeitsfläche/ecoliK12CHIP.db/',repos=NULL)







if(TRUE)
{

cat('\nErzeuge dummy Daten\n\n')	
dummy_data <- data.frame(sample(feature.seq.list[[1]],2000),rnorm(2000))

cat('\nFüge externe dummy Daten dem .db hinzu\n\n')
addNewAnnotation(x='~/Arbeitsfläche/ecoliK12CHIP.db/inst/extdata/ecoliK12CHIP.db', data=dummy_data,newTableName='dummy',data.colNames='dummy_id',mapTableName='probes')

cat('\nFüge neues Bimap-Objekt Dummy Daten hinzu\n\n')
addBimapObj('~/Arbeitsfläche/ecoliK12CHIP.db/inst/extdata/ecoliK12CHIP.db','DUMMY','probes','dummy')

cat('\nLasse mir die CreateStatements anzeigen um vorhandene Tablenamen herauszufinden\n\n')
print(.dbSchema(x='~/Arbeitsfläche/ecoliK12CHIP.db/inst/extdata/ecoliK12CHIP.db'))

cat('\nFüge ein paar HTML-Links zur Datenbank, die dann für die HTML-Links der IDs verwendet werden.\nDort wo die IDs normal im Link stehen wird als Platzhalter $ID verwendet\n\n')
setIdLink('~/Arbeitsfläche/ecoliK12CHIP.db/inst/extdata/ecoliK12CHIP.db', "kegg", "http://www.genome.jp/dbget-bin/www_bget?pathway:eco$ID")
setIdLink('~/Arbeitsfläche/ecoliK12CHIP.db/inst/extdata/ecoliK12CHIP.db', "genbank_id", "http://www.ncbi.nlm.nih.gov/entrez/query.fcgi?db=Nucleotide&cmd=search&term=$ID")
setIdLink('~/Arbeitsfläche/ecoliK12CHIP.db/inst/extdata/ecoliK12CHIP.db', "genes", "http://www.ncbi.nlm.nih.gov/entrez/query.fcgi?db=gene&cmd=Retrieve&dopt=Graphics&list_uids=$ID")
setIdLink('~/Arbeitsfläche/ecoliK12CHIP.db/inst/extdata/ecoliK12CHIP.db', "refseq", "http://www.ncbi.nlm.nih.gov/entrez/query.fcgi?db=Nucleotide&cmd=search&term=$ID")
setIdLink('~/Arbeitsfläche/ecoliK12CHIP.db/inst/extdata/ecoliK12CHIP.db', "ec", "http://www.genome.jp/dbget-bin/www_bget?ec:$ID")

cat('\nInstalliere neu erstellte .db Datenbank\n\n')	
install.packages(pkgs='~/Arbeitsfläche/ecoliK12CHIP.db/',repos=NULL)

cat('\nLade neu erstellte .db Datenbank und führe einige Funktionen aus\n\n')	
library(ecoliK12CHIP.db)

cat('\nEin paar Versuche...\n\n')
ecoliK12CHIPMAPCOUNTS

print(toTable(ecoliK12CHIPSEQUENCE)[1:10,])
print(toTable(ecoliK12CHIPDUMMY))

cat('\nGeneriere Dummy external data\n\n')
rprobe_ids <- feature.seq.list[sample(nrow(feature.seq.list),210),]
extdata <- data.frame(probe_id=rprobe_ids[[1]],extdata1=rnorm(210),extdata2=rnorm(210),seq_length=nchar(rprobe_ids[[2]]))

cat('\nGeneriere eine tolle HTML\n\n')
annotationPkgToHTML(ecoliK12CHIP_dbconn(),'Testing annotationPkgToHTML function','~/Arbeitsfläche/test',"probes",c("go_id","ec","genes","kegg","genbank_id","sequence","dummy"),rprobe_ids[[1]],extdata=extdata,colOrder=c("sequence","seq_length","probes","genes","genbank_id","kegg","ec","go_id","extdata1","extdata2","dummy"))

}



