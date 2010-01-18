## Translate the ecoliK12.db0 into a ecoliK12.db1
## Translation does not destroy any .db0 structure
## 
## only add additional tables

## TODO: Die restlichen Bimap Objekte hinzufügen und Metadata infos mit vererben. Requires und setGenerics auslagern
## Load Library
#require("RSQLite")

setGeneric("db1ConverterECOLIK12", function(connType,outputDir) standardGeneric("db1ConverterECOLIK12"))

## FilePath
setMethod("db1ConverterECOLIK12", signature("character","character"), function(connType,outputDir) 
{
	## Make copy of the database
	dbnamework <- paste(outputDir,".db1",sep="")
	file.copy(connType,dbnamework,overwrite=TRUE)
	
	## Load SQLite Driver
	drv <- dbDriver("SQLite")
	
	## Generate Connection Object	
	con <- dbConnect(drv, dbname = dbnamework)
	on.exit(dbDisconnect(con))
	
	## Call main function
	db1ConverterECOLIK12(con)
})

## SQLite-Connection
setMethod("db1ConverterECOLIK12", signature("SQLiteConnection","missing"), function(connType) 
{	
	## Tables with _id reference and their fieldnames and createstatements which are exported to .dbX
	tablenames <- c('probes_id','genbank_id','genes','refseq','ec','gene_info','gene_synonyms','kegg','pubmed','go_id')
	fieldnames <- c('probe_id','accession','gene_id','accession','ec_number','gene_name;symbol','symbol','path_id','pubmed_id','go_id;evidence;ontology')
	createstatements <- c('SELECT 1','CREATE TABLE genbank_id (_id integer REFERENCES internal_id(_id),accession TEXT NOT NULL)','CREATE TABLE genes (_id integer REFERENCES internal_id(_id),gene_id TEXT NOT NULL)','CREATE TABLE refseq (_id integer REFERENCES internal_id(_id),accession TEXT NOT NULL)','CREATE TABLE ec (_id integer REFERENCES internal_id(_id),ec_number TEXT NOT NULL)','CREATE TABLE gene_info (_id integer REFERENCES internal_id(_id),gene_name TEXT NOT NULL,symbol TEXT)','CREATE TABLE gene_synonyms (_id integer REFERENCES internal_id(_id),symbol TEXT NOT NULL)','CREATE TABLE kegg (_id integer REFERENCES internal_id(_id),path_id TEXT NOT NULL)','CREATE TABLE pubmed (_id integer REFERENCES internal_id(_id),pubmed_id TEXT NOT NULL)','CREATE TABLE go_id (_id INTEGER REFERENCES internal_id(_id) NOT NULL, go_id TEXT NOT NULL, evidence TEXT,ontology TEXT)')
	
	tb <- as.data.frame(cbind(tablenames,fieldnames,createstatements))
	
	colnames(tb) <- c('tablename','fieldnames','createstatements')
	dbWriteTable(connType, 'table_master_meta', tb, row.names = FALSE,overwrite = TRUE, append = FALSE)
	
	## Add Main ID table
	cat("Add Main ID table\n")
	sql <- "DROP TABLE IF EXISTS internal_id"
	dbGetQuery(connType,sql)
	sql <- "CREATE TABLE internal_id (_id INTEGER PRIMARY KEY)"
	dbGetQuery(connType,sql)
	
	## Writes internal ids into the database
	cat("Writes internal ids into the database\n")
	sql <- "INSERT INTO internal_id (_id) SELECT _id FROM genes"
	dbGetQuery(connType,sql)

	## Transform all GO Tables to the one Table go
	cat("Writes GO Table\n")
	sql <- "DROP TABLE IF EXISTS go_id"
	dbGetQuery(connType,sql)
	sql <- "CREATE TABLE go_id (_id INTEGER REFERENCES internal_id(_id) NOT NULL, go_id TEXT NOT NULL, evidence TEXT,ontology TEXT)"
	dbGetQuery(connType,sql)
	sql <- "INSERT INTO go_id  SELECT _id,go_id,evidence,'bp' FROM go_bp_all"
	dbGetQuery(connType,sql)
	sql <- "INSERT INTO go_id  SELECT _id,go_id,evidence,'mf' FROM go_mf_all"
	dbGetQuery(connType,sql)
	sql <- "INSERT INTO go_id  SELECT _id,go_id,evidence,'cc' FROM go_cc_all"
	dbGetQuery(connType,sql)
	
	## Add db1 Entrez-Table
	#cat("Writes Entrez Table\n")
	#sql <- "DROP TABLE IF EXISTS entrez_id"
	#dbGetQuery(connType,sql)
	#sql <- "CREATE TABLE entrez_id (_id integer REFERENCES internal_id(_id),value_id TEXT NOT NULL)"
	#dbGetQuery(connType,sql)
	#sql <- "INSERT INTO entrez_id (_id,value_id) SELECT _id,gene_id FROM genes"
	#dbGetQuery(connType,sql)
	
	## Add db1 Genbank-Table # Anmerkung in der Tabelle accessions sind genbank und refseq Ids. Genbank Ids sind nicht unique. Das ist auch richtig zB. ORFs, Phage insertions
	cat("Writes Genbank Table\n")
	sql <- "DROP TABLE IF EXISTS genbank_id"
	dbGetQuery(connType,sql)
	sql <- "CREATE TABLE genbank_id (_id integer REFERENCES internal_id(_id),accession TEXT NOT NULL)"
	dbGetQuery(connType,sql)
	sql <- "INSERT INTO genbank_id (_id,accession) SELECT DISTINCT _id,accession FROM accessions EXCEPT SELECT _id,accession FROM accessions WHERE accession LIKE '%*_%' ESCAPE '*'"
	dbGetQuery(connType,sql)
	
	## Add db1 Refseq-Table
	#cat("Writes Refseq Table\n")
	#sql <- "DROP TABLE IF EXISTS refseq_id"
	#dbGetQuery(connType,sql)
	#sql <- "CREATE TABLE refseq_id (_id integer REFERENCES internal_id(_id),value_id TEXT NOT NULL)"
	#dbGetQuery(connType,sql)
	#sql <- "INSERT INTO refseq_id (_id,value_id) SELECT _id,accession FROM refseq"
	#dbGetQuery(connType,sql)
	
	## Add db1 KEGG-Table
	#cat("Writes KEGG Table\n")
	#sql <- "DROP TABLE IF EXISTS kegg_id"
	#dbGetQuery(connType,sql)
	#sql <- "CREATE TABLE kegg_id (_id integer REFERENCES internal_id(_id),value_id TEXT NOT NULL)"
	#dbGetQuery(connType,sql)
	#sql <- "INSERT INTO kegg_id (_id,value_id) SELECT * FROM kegg"
	#dbGetQuery(connType,sql)
	
	## Add db1 pubmed-Table
	#cat("Writes pubmed Table\n")
	#sql <- "DROP TABLE IF EXISTS pubmed_id"
	#dbGetQuery(connType,sql)
	#sql <- "CREATE TABLE pubmed_id (_id integer REFERENCES internal_id(_id),value_id TEXT NOT NULL)"
	#dbGetQuery(connType,sql)
	#sql <- "INSERT INTO pubmed_id (_id,value_id) SELECT * FROM pubmed"
	#dbGetQuery(connType,sql)
	
	## Add db1 ec-Table
	#cat("Writes ec Table\n")
	#sql <- "DROP TABLE IF EXISTS ec_id"
	#dbGetQuery(connType,sql)
	#sql <- "CREATE TABLE ec_id (_id integer REFERENCES internal_id(_id),value_id TEXT NOT NULL)"
	#dbGetQuery(connType,sql)
	#sql <- "INSERT INTO ec_id (_id,value_id) SELECT * FROM ec"
	#dbGetQuery(connType,sql)
	
	## Add db1 gene_info-Table
	#cat("Writes gene_info Table\n")
	#sql <- "DROP TABLE IF EXISTS gene_info_id"
	#dbGetQuery(connType,sql)
	#sql <- "CREATE TABLE gene_info_id (_id integer REFERENCES internal_id(_id),value_id TEXT NOT NULL,attr1 TEXT)"
	#dbGetQuery(connType,sql)
	#sql <- "INSERT INTO gene_info_id (_id,value_id,attr1) SELECT * FROM gene_info"
	#dbGetQuery(connType,sql)
	
	## Add db1 symbol-Table
	#cat("Writes symbol Table\n")
	#sql <- "DROP TABLE IF EXISTS symbol_id"
	#dbGetQuery(connType,sql)
	#sql <- "CREATE TABLE symbol_id (_id integer REFERENCES internal_id(_id),value_id TEXT NOT NULL)"
	#dbGetQuery(connType,sql)
	#sql <- "INSERT INTO symbol_id (_id,value_id) SELECT * FROM gene_synonyms"
	#dbGetQuery(connType,sql)
	
	## Add bimap_meta Table
	cat("Writes bimap_meta Table\n")
	sql <- "DROP TABLE IF EXISTS bimap_meta"
	dbGetQuery(connType,sql)
	sql <- "CREATE TABLE IF NOT EXISTS bimap_meta (name TEXT PRIMARY KEY,table1 TEXT NOT NULL,table2 TEXT NOT NULL,tagname1 TEXT,tagname2 TEXT,comment TEXT,filter1 TEXT,filter2 TEXT)"
	dbGetQuery(connType,sql)
	sql <- paste("INSERT INTO bimap_meta VALUES ('ACCNUM','probes_id','genbank_id','','','','','')",sep="")
	dbGetQuery(connType,sql)
	sql <- paste("INSERT INTO bimap_meta VALUES ('ENTREZID','probes_id','genes','','','','','')",sep="")
	dbGetQuery(connType,sql)
	sql <- paste("INSERT INTO bimap_meta VALUES ('GO_ALL','probes_id','go_id','','Evidence=evidence;Ontology=ontology','','','')",sep="")
	dbGetQuery(connType,sql)
	sql <- paste("INSERT INTO bimap_meta VALUES ('GO_BP','probes_id','go_id','','Evidence=evidence','','','ontology =\"bp\"')",sep="")
	dbGetQuery(connType,sql)
	sql <- paste("INSERT INTO bimap_meta VALUES ('GO_MF','probes_id','go_id','','Evidence=evidence','','','ontology =\"mf\"')",sep="")
	dbGetQuery(connType,sql)
	sql <- paste("INSERT INTO bimap_meta VALUES ('GO_CC','probes_id','go_id','','Evidence=evidence','','','ontology =\"cc\"')",sep="")
	dbGetQuery(connType,sql)
	sql <- paste("INSERT INTO bimap_meta VALUES ('ALIAS','probes_id','gene_synonyms','','','','','')",sep="")
	dbGetQuery(connType,sql)
	sql <- paste("INSERT INTO bimap_meta VALUES ('ALIAS2PROBE','gene_synonyms','probes_id','','','','','')",sep="")
	dbGetQuery(connType,sql)
	sql <- paste("INSERT INTO bimap_meta VALUES ('ENZYME','probes_id','ec','','','','','')",sep="")
	dbGetQuery(connType,sql)
	sql <- paste("INSERT INTO bimap_meta VALUES ('ENZYME2PROBE','ec','probes_id','','','','','')",sep="")
	dbGetQuery(connType,sql)
	sql <- paste("INSERT INTO bimap_meta VALUES ('PATH','probes_id','kegg','','','','','')",sep="")
	dbGetQuery(connType,sql)
	sql <- paste("INSERT INTO bimap_meta VALUES ('PATH2PROBE','kegg','probes_id','','','','','')",sep="")
	dbGetQuery(connType,sql)
	sql <- paste("INSERT INTO bimap_meta VALUES ('PMID','probes_id','pubmed','','','','','')",sep="")
	dbGetQuery(connType,sql)
	sql <- paste("INSERT INTO bimap_meta VALUES ('PMID2PROBE','pubmed','probes_id','','','','','')",sep="")
	dbGetQuery(connType,sql)
	sql <- paste("INSERT INTO bimap_meta VALUES ('REFSEQ','probes_id','refseq','','','','','')",sep="")
	dbGetQuery(connType,sql)
	sql <- paste("INSERT INTO bimap_meta VALUES ('SYMBOL','probes_id','gene_info','','','','','')",sep="")
	dbGetQuery(connType,sql)
})
