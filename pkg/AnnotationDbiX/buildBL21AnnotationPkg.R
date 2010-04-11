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
