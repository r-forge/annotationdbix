## Translate the ecoliK12.db0 into a ecoliK12.db1
## 
## Translation does not destroy any .db0 structure - only add additional tables

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
	tablenames <- c(
		'genbank_id',
		'genes',
		'refseq',
		'ec',
		'gene_info',
		'gene_synonyms',
		'kegg',
		'pubmed',
		'go_id',
		'go_bp',
		'go_bp_all',
		'go_cc',
		'go_cc_all',
		'go_mf',
		'go_mf_all')
	
	fieldnames <- c(
		'accession',
		'gene_id',
		'accession',
		'ec_number',
		'gene_name;symbol',
		'symbol',
		'path_id',
		'pubmed_id',
		'go_id;evidence;ontology',
		'go_id;evidence',
		'go_id;evidence',
		'go_id;evidence',
		'go_id;evidence',
		'go_id;evidence',
		'go_id;evidence')
	
	linksURL <- c(
		'http://www.ncbi.nlm.nih.gov/entrez/query.fcgi?db=Nucleotide&cmd=search&term=$ID',
		'http://www.ncbi.nlm.nih.gov/entrez/query.fcgi?db=gene&cmd=Retrieve&dopt=Graphics&list_uids=$ID',
		'http://www.ncbi.nlm.nih.gov/entrez/query.fcgi?db=Nucleotide&cmd=search&term=$ID',
		'','|','','','','||','|','|','|','|','|','|')
		
	createstatements <- c(
		'CREATE TABLE genbank_id (_id integer NOT NULL,accession VARCHAR(20) NOT NULL,FOREIGN KEY (_id) REFERENCES internal_id(_id))',
		'CREATE TABLE genes (_id integer NOT NULL,gene_id VARCHAR(10) NOT NULL,FOREIGN KEY (_id) REFERENCES internal_id(_id))',
		'CREATE TABLE refseq (_id integer NOT NULL,accession VARCHAR(20) NOT NULL,FOREIGN KEY (_id) REFERENCES internal_id(_id))',
		'CREATE TABLE ec (_id integer NOT NULL,ec_number VARCHAR(13) NOT NULL,FOREIGN KEY (_id) REFERENCES internal_id(_id))',
		'CREATE TABLE gene_info (_id integer NOT NULL,gene_name VARCHAR(255) NOT NULL,symbol VARCHAR(80),FOREIGN KEY (_id) REFERENCES internal_id(_id))',
		'CREATE TABLE gene_synonyms (_id integer NOT NULL,symbol VARCHAR(80) NOT NULL,FOREIGN KEY (_id) REFERENCES internal_id(_id))',
		'CREATE TABLE kegg (_id integer NOT NULL,path_id CHAR(5) NOT NULL,FOREIGN KEY (_id) REFERENCES internal_id(_id))',
		'CREATE TABLE pubmed (_id integer NOT NULL,pubmed_id VARCHAR(10) NOT NULL,FOREIGN KEY (_id) REFERENCES internal_id(_id))',
		'CREATE TABLE go_id (_id INTEGER NOT NULL, go_id CHAR(10) NOT NULL, evidence CHAR(3),ontology VARCHAR(9),FOREIGN KEY (_id) REFERENCES internal_id(_id))',
		'CREATE TABLE go_bp (_id INTEGER NOT NULL, go_id CHAR(10) NOT NULL, evidence CHAR(3),FOREIGN KEY (_id) REFERENCES internal_id(_id))',
		'CREATE TABLE go_bp_all (_id INTEGER NOT NULL, go_id CHAR(10) NOT NULL, evidence CHAR(3),FOREIGN KEY (_id) REFERENCES internal_id(_id))',
 		'CREATE TABLE go_cc (_id INTEGER NOT NULL, go_id CHAR(10) NOT NULL, evidence CHAR(3),FOREIGN KEY (_id) REFERENCES internal_id(_id))',
 		'CREATE TABLE go_cc_all (_id INTEGER NOT NULL, go_id CHAR(10) NOT NULL, evidence CHAR(3),FOREIGN KEY (_id) REFERENCES internal_id(_id))',
 		'CREATE TABLE go_mf (_id INTEGER NOT NULL, go_id CHAR(10) NOT NULL, evidence CHAR(3),FOREIGN KEY (_id) REFERENCES internal_id(_id))',
 		'CREATE TABLE go_mf_all (_id INTEGER NOT NULL, go_id CHAR(10) NOT NULL, evidence CHAR(3),FOREIGN KEY (_id) REFERENCES internal_id(_id))')
	
	tb <- as.data.frame(cbind(tablenames,fieldnames,createstatements,linksURL))
	
	colnames(tb) <- c('tablename','fieldnames','createstatements','links')
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

	## Transform all GO tables to the table go_id with one additional column ontology
	cat("Writes GO Table\n")
	sql <- "DROP TABLE IF EXISTS go_id"
	dbGetQuery(connType,sql)
	
	sql <- "CREATE TABLE go_id (_id INTEGER NOT NULL, go_id CHAR(10) NOT NULL, evidence CHAR(3),ontology VARCHAR(9),FOREIGN KEY (_id) REFERENCES internal_id(_id))"
	dbGetQuery(connType,sql)
	
	sql <- "INSERT INTO go_id  SELECT _id,go_id,evidence,'bp' FROM go_bp_all"
	dbGetQuery(connType,sql)
	
	sql <- "INSERT INTO go_id  SELECT _id,go_id,evidence,'mf' FROM go_mf_all"
	dbGetQuery(connType,sql)
	
	sql <- "INSERT INTO go_id  SELECT _id,go_id,evidence,'cc' FROM go_cc_all"
	dbGetQuery(connType,sql)
	
	## Add db1 Genbank-Table with correct values
	cat("Writes Genbank Table\n")
	sql <- "DROP TABLE IF EXISTS genbank_id"
	dbGetQuery(connType,sql)
	
	sql <- "CREATE TABLE genbank_id (_id integer NOT NULL,accession VARCHAR(20) NOT NULL,FOREIGN KEY (_id) REFERENCES internal_id(_id))"
	dbGetQuery(connType,sql)
	
	sql <- "INSERT INTO genbank_id (_id,accession) SELECT DISTINCT _id,accession FROM accessions EXCEPT SELECT _id,accession FROM accessions WHERE accession LIKE '%*_%' ESCAPE '*'"
	dbGetQuery(connType,sql)
	
	## Add bimap_meta tables
	cat("Writes bimap_meta Table\n")
	sql <- "DROP TABLE IF EXISTS bimap_meta"
	dbGetQuery(connType,sql)
	sql <- "CREATE TABLE IF NOT EXISTS bimap_meta (name VARCHAR(40) PRIMARY KEY,table1 VARCHAR(40) NOT NULL,table2 VARCHAR(40) NOT NULL,
		tagname1 VARCHAR(40),tagname2 VARCHAR(40),comment VARCHAR(40),filter1 VARCHAR(255),filter2 VARCHAR(255),revmap INTEGER)"
	dbGetQuery(connType,sql)

	sql <- paste("INSERT INTO bimap_meta VALUES ('ACCNUM','probes','genbank_id','','','','','',1)",sep="")
	dbGetQuery(connType,sql)

	sql <- paste("INSERT INTO bimap_meta VALUES ('ENTREZID','probes','genes','','','','','',1)",sep="")
	dbGetQuery(connType,sql)

	sql <- paste("INSERT INTO bimap_meta VALUES ('GO_ALL','probes','go_id','','Ontology=ontology','','','',1)",sep="")
	dbGetQuery(connType,sql)

	sql <- paste("INSERT INTO bimap_meta VALUES ('GO_BP','probes','go_bp','','Evidence=evidence','','','',1)",sep="")
	dbGetQuery(connType,sql)

	sql <- paste("INSERT INTO bimap_meta VALUES ('GO_MF','probes','go_mf','','Evidence=evidence','','','',1)",sep="")
	dbGetQuery(connType,sql)

	sql <- paste("INSERT INTO bimap_meta VALUES ('GO_CC','probes','go_cc','','Evidence=evidence','','','',1)",sep="")
	dbGetQuery(connType,sql)
	
	sql <- paste("INSERT INTO bimap_meta VALUES ('GO_BP_ALL','probes','go_bp_all','','Evidence=evidence','','','',1)",sep="")
	dbGetQuery(connType,sql)

	sql <- paste("INSERT INTO bimap_meta VALUES ('GO_MF_ALL','probes','go_mf_all','','Evidence=evidence','','','',1)",sep="")
	dbGetQuery(connType,sql)

	sql <- paste("INSERT INTO bimap_meta VALUES ('GO_CC_ALL','probes','go_cc_all','','Evidence=evidence','','','',1)",sep="")
	dbGetQuery(connType,sql)

	sql <- paste("INSERT INTO bimap_meta VALUES ('ALIAS','probes','gene_synonyms','','','','','',1)",sep="")
	dbGetQuery(connType,sql)

	sql <- paste("INSERT INTO bimap_meta VALUES ('ALIAS2PROBE','probes','gene_synonyms','','','','','',-1)",sep="")
	dbGetQuery(connType,sql)

	sql <- paste("INSERT INTO bimap_meta VALUES ('ENZYME','probes','ec','','','','','',1)",sep="")
	dbGetQuery(connType,sql)

	sql <- paste("INSERT INTO bimap_meta VALUES ('ENZYME2PROBE','probes','ec','','','','','',-1)",sep="")
	dbGetQuery(connType,sql)

	sql <- paste("INSERT INTO bimap_meta VALUES ('PATH','probes','kegg','','','','','',1)",sep="")
	dbGetQuery(connType,sql)

	sql <- paste("INSERT INTO bimap_meta VALUES ('PATH2PROBE','probes','kegg','','','','','',-1)",sep="")
	dbGetQuery(connType,sql)

	sql <- paste("INSERT INTO bimap_meta VALUES ('PMID','probes','pubmed','','','','','',1)",sep="")
	dbGetQuery(connType,sql)

	sql <- paste("INSERT INTO bimap_meta VALUES ('PMID2PROBE','probes','pubmed','','','','','',-1)",sep="")
	dbGetQuery(connType,sql)

	sql <- paste("INSERT INTO bimap_meta VALUES ('REFSEQ','probes','refseq','','','','','',1)",sep="")
	dbGetQuery(connType,sql)

	sql <- paste("INSERT INTO bimap_meta VALUES ('SYMBOL','probes','gene_info','','','','','',1)",sep="")
	dbGetQuery(connType,sql)
	
	return(TRUE)
})
