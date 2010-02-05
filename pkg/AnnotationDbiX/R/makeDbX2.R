## Generates a .dbX package

#require("RSQLite")
#require("AnnotationDbi")
#require("AnnotationDbiX")
	
setGeneric("makeDbX2", signature = c("probeList","organism","species","prefix","outputDir","version","chipName","author","maintainer"),
	function(probeList,organism,species,prefix,outputDir,version,chipName,author,maintainer,manufacturer="Manufacturer not specified",manufacturerUrl="ManufacturerUrl not specified",tableName='sequence',colName='sequence') standardGeneric("makeDbX2"))
	
## FilePath
setMethod("makeDbX2",
signature("data.frame","character","character","character","character","character","character","character","character"), 
function(probeList,organism,species,prefix,outputDir,version,chipName,author,maintainer,manufacturer,manufacturerUrl,tableName,colName) 
{	
	
	## Test parameters
	if(dim(probeList)[2] != 2)
		stop("'probeList' must have 2 columns. The left side must be the featurenames and the right side must be a unique identifier e.g. oligosequence.")
	
	if(nrow(unique(probeList)) != nrow(unique(probeList[1])))
		stop("'probeList' is not unique. Same featurename must have same id.")
	
	if(colName == 'probe_id')
		stop("'colName' must not be named 'probe_id'.")

	if(any(is.na(unique(probeList))))
		stop("'probeList' must not contain NAs.")
				
	## Prepare .dbX creation
	template_path <- system.file("Pkg-template",package="AnnotationDbiX")
                 
    ann_dbi_version <- installed.packages()['AnnotationDbi','Version']
    
	symvals <- list(
			#DBSCHEMA=x@DBschema,
			PKGTITLE=paste(manufacturer,prefix,"annotation data (",chipName,")"),
			ANNOBJPREFIX=prefix,
			ANNOBJTARGET=chipName,
			ORGANISM=organism,
			SPECIES=species,
			MANUF=manufacturer,
			CHIPNAME=chipName,
			MANUFURL=manufacturerUrl,
			AUTHOR=paste(author,collapse=', '),
			MAINTAINER=paste(maintainer,collapse=', '),
			PKGVERSION=version,
			LIC='Artistic-2.0',
			#BIOCVIEWS=x@biocViews,
			#DBFILE=chipSrc,
			DBFILENEW=paste(prefix,".dbX",sep=""),
			ANNDBIVERSION=ann_dbi_version
        )
        
    ## Create Package
	createPackage(paste(prefix,'.dbX',sep=""),
					destinationDir=outputDir,
					originDir=template_path,
					symbolValues=symvals,
					unlink=TRUE)                            
	                            
	## Load SQLite Driver
	drv <- dbDriver("SQLite")
	
	## Generate Connection Object	
	db_dir <- file.path(outputDir,paste(prefix,'.dbX',sep=""),'inst','extdata',prefix)
	cat('Generates a sqlite file at',db_dir,'\n')
	con <- dbConnect(drv, dbname = paste(db_dir,".dbX",sep=""))
	on.exit(dbDisconnect(con))
	
	## Drop existing tables
	#sql <- "DROP TABLE IF EXISTS internal_id"
	#dbGetQuery(con,sql)
	sql <- paste("DROP TABLE IF EXISTS",tableName)
	dbGetQuery(con,sql)
	sql <- "DROP TABLE IF EXISTS meta"
	dbGetQuery(con,sql)
	sql <- "DROP TABLE IF EXISTS bimap_meta"
	dbGetQuery(con,sql)
	sql <- "DROP TABLE IF EXISTS probes_id"
	dbGetQuery(con,sql)
	sql <- "DROP TABLE IF EXISTS table_master_meta"
	dbGetQuery(con,sql)
	
	## Add helper table	
	cat("Add helper Table\n")
	colnames(probeList) <- (c("probe_id",colName))
	dbWriteTable(conn=con,name="probes_temp",value=unique(probeList),row.names=FALSE,overwrite=TRUE)
	
	## Add meta Table
	cat("Add meta Table\n")
	sql <- "CREATE TABLE meta (key TEXT PRIMARY KEY, value TEXT NOT NULL)"
	dbGetQuery(con,sql)
	
	## Add table_master_meta
	cat("Add table_master_meta\n")
	sql <- "CREATE TABLE table_master_meta (tablename TEXT, fieldnames TEXT)"
	dbGetQuery(con,sql)
	
	## Add Bimap table
	#cat("Add bimap table\n")
	#sql<-"CREATE TABLE bimap_meta(name TEXT PRIMARY KEY,table1 TEXT NOT NULL,table2 TEXT NOT NULL,tagname1 TEXT,tagname2 TEXT,comment TEXT,filter1 TEXT,filter2 TEXT)"
	#dbGetQuery(con,sql)
	
	## Add User Defined table
	cat("Add",tableName,"table\n")
	sql <- paste("CREATE TABLE",tableName,"(_id INTEGER PRIMARY KEY,",colName,"TEXT NOT NULL)")
	dbGetQuery(con,sql)
	
	## Add Main ID table
	#cat("Add Main ID table\n")
	#sql <- paste("CREATE TABLE internal_id (_id INTEGER PRIMARY KEY)")
	#dbGetQuery(con,sql)
	
	## Add Probes table
	cat("Add probes table\n")
	sql <- paste("CREATE TABLE probes (_id INTEGER,probe_id TEXT)")
	dbGetQuery(con,sql)
	
	## Fill User Defined table
	cat("Fill",tableName,"table\n")
	sql <- paste("INSERT INTO",tableName,"SELECT NULL,",colName,"FROM probes_temp GROUP BY",colName)
	dbGetQuery(con,sql)
	
	## Fill internal ID table
	#cat("Fill internal ID table\n")
	#sql <- paste("INSERT INTO internal_id SELECT _id FROM",tableName)
	#dbGetQuery(con,sql)
	
	## Fill Probes table
	cat("Fill Probes table\n")
	sql <- paste("INSERT INTO probes SELECT _id,probe_id FROM probes_temp p,",tableName," s WHERE p.",colName," = s.",colName," GROUP BY p.probe_id",seq="")
	dbGetQuery(con,sql)
	
	## Fill meta Table
	cat("Fill meta Table\n")
	sql <- paste("INSERT INTO meta (key,value) VALUES ('main_table','",tableName,"')")
	dbGetQuery(con,sql)
	
	## Fill meta Table
	cat("Fill meta Table\n")
	sql <- paste("INSERT INTO table_master_meta (tablename,fieldnames) VALUES ('probes','probe_id')")
	dbGetQuery(con,sql)
	sql <- paste("INSERT INTO table_master_meta (tablename,fieldnames) VALUES ('",tableName,"','",colName,"')",sep="")
	dbGetQuery(con,sql)
	
	## Remove Helper table
	sql <- "DROP TABLE IF EXISTS probes_temp"
	dbGetQuery(con,sql)	
})


