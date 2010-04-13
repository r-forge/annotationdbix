BL21_data1 <- read.table(file='~/Projekte/Diplomarbeit/data/Peter/ecocyc_accn_export.txt',header=FALSE,sep="\t",stringsAsFactors=FALSE)
BL21_data2 <- read.table(file='~/Projekte/Diplomarbeit/data/Peter/ecocyc_go_export.txt',header=FALSE,sep="\t",stringsAsFactors=FALSE)
BL21_gal <- read.table(file='~/Projekte/Diplomarbeit/data/gal/acbt_ecoli_bl21_2007_rev20090928.gal',header=TRUE,sep="\t",stringsAsFactors=FALSE)

colnames(BL21_data1) <- c("EcoCyc_Gene_Accn","Database","Accn")
colnames(BL21_data2) <- c("EcoCyc_Gene_Accn","GO_ID","Ontology","Evidence")

BL21_ASAP <- BL21_data1[BL21_data1[['Database']] == 'ASAP',c(1,3)]
BL21_Blattner <- BL21_data1[BL21_data1[['Database']] == 'BlattnerID',c(1,3)]
BL21_UniProt <- BL21_data1[BL21_data1[['Database']] == 'UNIPROT',c(1,3)]
BL21_Refseq <- BL21_data1[BL21_data1[['Database']] == 'REFSEQ',c(1,3)]
BL21_GO <- BL21_data2[1:3]

# Extract evidence code
BL21_GO <- cbind(BL21_GO,Evidence=apply(BL21_data2[4],1,function(x) {strsplit(x,":")[[1]][2]}))

# Rename long ontology name to short ontology name
BL21_GO[BL21_GO[3] == "molecular_function",3] = "mf"
BL21_GO[BL21_GO[3] == "cellular_component",3] = "cc"
BL21_GO[BL21_GO[3] == "biological_process",3] = "bp"

# Delete all rows without a sequence
BL21_gal <- BL21_gal[!BL21_gal['OligoSequence'] == "",]

library(AnnotationDbiX)

# Generates main .dbX annotation package with 'probe_ids' and 'sequence'
makeDbX(BL21_gal[c("ID","OligoSequence")],'Eschericha Coli','EColi','ecoliBL21CHIP','~/Desktop','0.1.0','Mo-Ferm','ecoliBL21 Chip','www.derauer.net',tableName='oligo_sequence',author=c('Norbert Auer'),maintainer='Norbert Auer <norbert@derauer.net>')

# Add other columns from the .gal file to annotation package
addNewAnnotation(x='~/Desktop/ecoliBL21CHIP.dbX/inst/extdata/ecoliBL21CHIP.dbX', data=BL21_gal[c('ID','LocusTag')],newTableName='locus_tag',data.colNames='locus_tag',mapTableName='probes')

addNewAnnotation(x='~/Desktop/ecoliBL21CHIP.dbX/inst/extdata/ecoliBL21CHIP.dbX', data=BL21_gal[c('ID','EntrezGeneID')],newTableName='genes',data.colNames='gene_id',mapTableName='probes')

addNewAnnotation(x='~/Desktop/ecoliBL21CHIP.dbX/inst/extdata/ecoliBL21CHIP.dbX', data=BL21_gal[c('ID','GenBankAccn')],newTableName='genbank_id',data.colNames='accession',mapTableName='probes')

addNewAnnotation(x='~/Desktop/ecoliBL21CHIP.dbX/inst/extdata/ecoliBL21CHIP.dbX', data=BL21_gal[c('ID','SwissProtID')],newTableName='swissprot_id',data.colNames='swissprot_id',mapTableName='probes')

# Add some Bimaps
addBimapObj('~/Desktop/ecoliBL21CHIP.dbX/inst/extdata/ecoliBL21CHIP.dbX','SEQUENCE','probes','oligo_sequence')
addBimapObj('~/Desktop/ecoliBL21CHIP.dbX/inst/extdata/ecoliBL21CHIP.dbX','LOCUS','probes','locus_tag')
addBimapObj('~/Desktop/ecoliBL21CHIP.dbX/inst/extdata/ecoliBL21CHIP.dbX','ENTREZ','probes','genes')
addBimapObj('~/Desktop/ecoliBL21CHIP.dbX/inst/extdata/ecoliBL21CHIP.dbX','ACCNUM','probes','genbank_id')
addBimapObj('~/Desktop/ecoliBL21CHIP.dbX/inst/extdata/ecoliBL21CHIP.dbX','SWISS','probes','swissprot_id')

# Add some Links for the HTML function
setIdLink('~/Desktop/ecoliBL21CHIP.dbX/inst/extdata/ecoliBL21CHIP.dbX', "genbank_id", "http://www.ncbi.nlm.nih.gov/entrez/query.fcgi?db=Nucleotide&cmd=search&term=$ID")
setIdLink('~/Desktop/ecoliBL21CHIP.dbX/inst/extdata/ecoliBL21CHIP.dbX', "genes", "http://www.ncbi.nlm.nih.gov/entrez/query.fcgi?db=gene&cmd=Retrieve&dopt=Graphics&list_uids=$ID")

install.packages(pkgs='~/Desktop/ecoliBL21CHIP.dbX/',repos=NULL)

library(ecoliBL21CHIP.dbX)

ecoliBL21CHIPMAPCOUNTS

