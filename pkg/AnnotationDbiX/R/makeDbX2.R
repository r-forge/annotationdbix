## Generate a .db.sqlite from a .db1.sqlite 
debug = FALSE

require("RSQLite")
require("AnnotationDbi")
require("AnnotationDbiX")
	
setGeneric("makeDbX2", 
	function(probeList,organism,species,prefix,chipSrc,baseMapTableName,outputDir,version,manufacturer,chipName,manufacturerUrl,author,maintainer) standardGeneric("makeDbX2"))
	
## FilePath
setMethod("makeDbX2",
signature("data.frame","character","character","character","character","character","character","character","character","character","character","character","character"), 
function(probeList,organism,species,prefix,chipSrc,baseMapTableName,outputDir,version,manufacturer="Manufacturer not specified" ,chipName,manufacturerUrl,author,maintainer) 
{	
	
	
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
			DBFILE=chipSrc,
			DBFILENEW=paste(prefix,".dbX",sep=""),
			ANNDBIVERSION=ann_dbi_version
        )
        
    ## Create Package
	createPackage(paste(prefix,'.dbX',sep=""),
					destinationDir=outputDir,
					originDir=template_path,
					symbolValues=symvals,
					unlink=TRUE)
                            
	## Load table_master_info
	#cat('Load table_master_info\n')
	#tableinfo <- testDb1(chipSrc,baseMapTableName)
	#tableinfo <- cbind(tableinfo,apply(tableinfo[2],1,function(x) strsplit(x,";")[[1]][1]))
	                            
	## Load SQLite Driver
	drv <- dbDriver("SQLite")
	
	## Generate Connection Object	
	db_dir <- file.path(outputDir,paste(prefix,'.dbX',sep=""),'inst','extdata',prefix)
	cat('Generate sqlite file at',db_dir)
	con <- dbConnect(drv, dbname = paste(db_dir,".dbX",sep=""))
	on.exit(dbDisconnect(con))
	
	## Drop existing tables
	sql <- "DROP TABLE IF EXISTS internal_id"
	dbGetQuery(con,sql)
	sql <- "DROP TABLE IF EXISTS probes_id"
	dbGetQuery(con,sql)
	sql <- "DROP TABLE IF EXISTS table_master_meta"
	dbGetQuery(con,sql)
	
	## Attach Database
	#cat('Attach Database',chipSrc,'as db1\n')
	#sql <- paste("ATTACH '",chipSrc,"' AS db1",sep="")
	#dbGetQuery(con,sql)

	## Get colinfo
	#sql <- paste("PRAGMA table_info(",baseMapTableName,")",sep="")
	#colinfo <- dbGetQuery(con,sql)
	
	## Add helper table	
	#colnames(probeList) <- (c("probe_id",colinfo[-1,'name']))
	#dbWriteTable(conn=con,name="probes_temp",value=unique(probeList),row.names=FALSE,overwrite=TRUE)

	## Add Main ID table
	#cat("Add Main ID table\n")
	#dyn <- paste(apply(colinfo[-1,],1,function(x) paste(x['name'],x['type'])),collapse=",")
	#sql <- paste("CREATE TABLE internal_id (_id INTEGER PRIMARY KEY,db1_id INTEGER,",dyn,")")
	#dbGetQuery(con,sql)

	#id_name <- tableinfo[tableinfo[1] == baseMapTableName,4]
	
	## Fill internal ID table for mapping to .db1
	#cat("Fill internal ID table for mapping to .db1\n")
	#dyn <- paste(colinfo[-1,'name'],collapse=",")	
	#sql <- paste("INSERT INTO internal_id (db1_id,",dyn,") SELECT DISTINCT l.* FROM probes_temp p, db1.",baseMapTableName," l WHERE l.",id_name," = p.",id_name,sep="")
	#dbGetQuery(con,sql)
	
	if(debug)
	{
	## Add Probe Table
	cat("Add probes table\n")
	sql <- "CREATE TABLE probes_id (_id INTEGER NOT NULL REFERENCES id(_id),probe_id TEXT NOT NULL)"
	dbGetQuery(con,sql)
	
	## Fill probes table with _id in .db1
	cat("Fill probes table with _id in .db1\n")
	sql <- paste("INSERT INTO probes_id (_id,probe_id) SELECT i._id,p.probe_id FROM internal_id i,probes_temp p WHERE p.",colinfo[-1,'name']," = i.",colinfo[-1,'name'],sep="")
	dbGetQuery(con,sql)
	
	## Update internal_id table with values only in list
	cat("Update internal_id table with values only in list\n")
	dyn <- paste(colinfo[-1,'name'],collapse=",")
	dyn1 <- paste(paste("p.",colinfo[-1,'name'],sep=""),collapse=",")
	sql <- paste("INSERT INTO internal_id (",dyn,") SELECT ",dyn," FROM (SELECT ",dyn1," FROM probes_temp p EXCEPT SELECT ",dyn1," FROM probes_temp p,internal_id i WHERE p.",id_name," = i.",id_name,") WHERE ",id_name," IS NOT NULL",sep="")
	dbGetQuery(con,sql)
	
	## Fill probes table with _ids only in list
	cat("Fill probes table with _ids only in list\n")
	sql <- paste("INSERT INTO probes_id (_id,probe_id) SELECT _id,p.probe_id FROM probes_temp p,internal_id i WHERE i.db1_id IS NULL AND i.",id_name," = p.",id_name,sep="")
	dbGetQuery(con,sql)
	
	## Fill probes table with probe_ids from list with no internal_id
	#cat("Fill probes table with probe_ids from list with no internal_id\n")
	#sql <- paste("INSERT INTO probes_id (_id,probe_id) SELECT NULL,probe_id FROM probes_temp p WHERE p.",id_name," IS NULL",sep="")
	#dbGetQuery(con,sql)
	
	## Detach database for creating multiple tables_id
	sql <- "DETACH db1"
	dbGetQuery(con,sql)
	
	## Add table_master_meta
	cat("Add table_master_meta\n")
	sql <- "CREATE TABLE table_master_meta (tablename TEXT, fieldnames TEXT, createstatements TEXT)"
	dbGetQuery(con,sql)
	
	## Add all other xxx_id tables
	cat("Add all other xxx_id tables\n")
	for(i in 2:nrow(tableinfo)) # 1.table is probes_id already exist
	{
		dbGetQuery(con,paste("DROP TABLE IF EXISTS",tableinfo[i,1]))	
		dbGetQuery(con,tableinfo[i,3])
	}
	
	## Add bimap_meta Table if exists
	is.bimap <- testBimapMeta(chipSrc)
	if(is.bimap)
	{
		cat("Add bimap_meta Table\n")
		sql <- "DROP TABLE IF EXISTS bimap_meta"
		dbGetQuery(con,sql)
		sql<-"CREATE TABLE IF NOT EXISTS bimap_meta(name TEXT PRIMARY KEY,table1 TEXT NOT NULL,table2 TEXT NOT NULL,tagname1 TEXT,tagname2 TEXT,comment TEXT,filter1 TEXT,filter2 TEXT)"
		dbGetQuery(con,sql)
	}
	
	## Attach Database
	sql <- paste("ATTACH '",chipSrc,"' AS db1",sep="")
	dbGetQuery(con,sql)
	
	## Fill bimap_meta Table if exists
	if(is.bimap)
	{
		sql <- "INSERT INTO bimap_meta SELECT * FROM db1.bimap_meta"
		dbGetQuery(con,sql)
	}
	
	## Fill table_master_meta
	cat("Fill table_master_meta\n")
	sql <- "INSERT INTO table_master_meta SELECT * FROM db1.table_master_meta"
	dbGetQuery(con,sql)
	
	## Fill baseMapTableName table
	#cat("Fill",baseMapTableName,"table\n")
	#dyn <- paste(colinfo[,'name'],collapse=",")
	#sql <- paste("INSERT INTO",baseMapTableName,"SELECT",dyn,"FROM internal_id")
	#dbGetQuery(con,sql)
	
	## Fill all other _id tables
	sapply(tableinfo[-1,][[1]],function(x) 
	{
		cat("Fill",x,"table\n")
		## Get colinfo
		sql <- paste("PRAGMA table_info(",x,")",sep="")
		colinfo <- dbGetQuery(con,sql)
		
		dyn <- paste(paste("l.",colinfo[-1,'name'],sep=""),collapse=",")
		sql <- paste("INSERT INTO ",x," SELECT i._id,",dyn," FROM internal_id i,db1.",x," l WHERE l._id = i.db1_id",sep="")
		dbGetQuery(con,sql)
	})
	
	## Creates index for all _id tables
	cat("Creates indexes for all _id tables\n")
	for(i in 1:nrow(tableinfo)) # 1.table is probes_id already exist
	{
		sql <- paste("CREATE INDEX F",tableinfo[i,1]," ON ",tableinfo[i,1],"(_id)",sep="")
		dbGetQuery(con,sql)	
	}
	
	## Remove Helper table
	sql <- "DROP TABLE IF EXISTS probes_temp"
	dbGetQuery(con,sql)
	
	
	## Detach Database
	sql <- "DETACH db1"
	dbGetQuery(con,sql)
	
	## Removed helper columns from internal_id -- There is no ALTER TABLE DROP column in SQLite
	sql <- "ALTER TABLE internal_id RENAME TO id_temp"
	dbGetQuery(con,sql)
	
	sql <- "CREATE TABLE internal_id (_id INTEGER PRIMARY KEY)"
	dbGetQuery(con,sql)
	
	sql <- "INSERT INTO internal_id SELECT _id FROM id_temp"
	dbGetQuery(con,sql)
	} ## DEBUG
})

testDb1 <- function(dbfile,baseMapTableName)
{
	## Load SQLite Driver
	drv <- dbDriver("SQLite")
	
	## Generate Connection Object	
	con <- dbConnect(drv, dbname = dbfile)
	
	if(!dbExistsTable(con,baseMapTableName))
		stop("baseMapTableName '",baseMapTableName,"' is not valid")
		
	sql <- "SELECT * FROM table_master_meta"
	table_master_info <- dbGetQuery(con,sql)

	for(i in 2:nrow(table_master_info)) # 1. row is probes_id table, only in dbX
		if(!dbExistsTable(con,table_master_info[i,'tablename']))
			stop(table_master_info[i,'tablename'],"' is not valid")

	if(!dbExistsTable(con,'bimap_meta'))
	
	dbDisconnect(con)
	
	return(table_master_info)
}

testBimapMeta <- function(dbfile)
{
	## Load SQLite Driver
	drv <- dbDriver("SQLite")
	
	## Generate Connection Object	
	con <- dbConnect(drv, dbname = dbfile)
	
	trig <- dbExistsTable(con,'bimap_meta')
	
	dbDisconnect(con)
	
	return(trig)
}
