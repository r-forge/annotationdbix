## Generates a .dbX package

setGeneric("makeDbX",signature = c("probeList","organism","species","prefix","outputDir","version","chipName","author","maintainer"),function(probeList,organism,species,prefix,outputDir,version,chipName,author,maintainer,manufacturer="Manufacturer not specified",manufacturerUrl="ManufacturerUrl not specified",newTableName='sequence',colName='sequence',bimapName,link='') standardGeneric("makeDbX"))
	
## FilePath
setMethod("makeDbX",signature("data.frame","character","character","character","character","character","character","character","character"),function(probeList,organism,species,prefix,outputDir,version,chipName,author,maintainer,manufacturer,manufacturerUrl,newTableName,colName,bimapName,link) 
{	
	
	## Test parameters
	if(dim(probeList)[2] != 2)
		stop("'probeList' must have 2 columns. The left side must be the featurenames and the right side must be a unique identifier e.g. oligosequence.")
	
	if(nrow(unique(probeList)) != nrow(unique(probeList[1])))
		stop("'probeList' is not unique. Same featurename must have same id.")
	
	if(colName == 'probe_id' || colName == '_id')
		stop("'colName' must not be named 'probe_id'.")
		
	if(newTableName == 'probes_temp' || newTableName == 'probes')
		stop("'newTableName' must not be named 'probes_temp' or 'probes'.")
		
	if(grepl('meta',newTableName))
		stop("'newTableName' must not contain 'meta' in the name.")

	if(any(is.na(unique(probeList))))
		stop("'probeList' must not contain NAs.")
		
	if(!missing(bimapName))
		if(is.character(bimapName))
		{
			if(length(bimapName) > 1)
				stop(paste("length(",bimapName,") must be 1.",sep=""))
		}
		else
			stop(paste("'",bimapName,"' must be from type 'character'.",sep=""))
				
	## Prepare .dbX creation
	template_path <- system.file("Pkg-template",package="AnnotationDbiX")
                 
    ann_dbi_version <- installed.packages()['AnnotationDbi','Version']
    
	symvals <- list(
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
		DBFILENEW=paste(prefix,".db",sep=""),
		ANNDBIVERSION=ann_dbi_version)
    
    ## Package Pathname
    pkgName <- paste(prefix,'.db',sep="")
    pkgPathName <- file.path(outputDir,pkgName)
    
    ## Test if package already exists
	if(file.exists(pkgPathName))
		stop('There already exists a package at ',pkgPathName,'. Remove it before.\n')
		
    ## Create Package
    cat('Create a new package at',pkgPathName,'.\n')
	createPackage(pkgName,
		destinationDir=outputDir,
		originDir=template_path,
		symbolValues=symvals)                            
	                            
	## Load SQLite Driver
	drv <- dbDriver("SQLite")
	
	## Generate Connection Object	
	db_dir <- file.path(pkgPathName,'inst','extdata',paste(prefix,'.db',sep=""))
	
	cat('Generates a sqlite file at',db_dir,'\n')
	con <- dbConnect(drv,dbname = db_dir)
	on.exit(dbDisconnect(con))
	
	## Add helper table	
	cat("Add helper Table\n")
	colnames(probeList) <- (c("probe_id",colName))
	dbWriteTable(conn=con,name="probes_temp",value=unique(probeList),row.names=FALSE,overwrite=TRUE)
	
	## Add meta Table
	cat("Add meta Table\n")
	sql <- "CREATE TABLE meta (key VARCHAR(80) PRIMARY KEY,value VARCHAR(80) NOT NULL)"
	dbGetQuery(con,sql)
	
	## Add table_master_meta
	cat("Add table_master_meta\n")
	sql <- "CREATE TABLE table_master_meta (tablename VARCHAR(80),fieldnames VARCHAR(80),links VARCHAR(255))"
	dbGetQuery(con,sql)
	
	## Add User Defined table
	cat("Add",newTableName,"table\n")
	sql <- paste("CREATE TABLE",newTableName,"(_id INTEGER PRIMARY KEY,",colName,"VARCHAR(255) NOT NULL)")
	dbGetQuery(con,sql)
	
	## Add Probes table
	cat("Add probes table\n")
	sql <- paste("CREATE TABLE probes (_id INTEGER,probe_id VARCHAR(80) NOT NULL)")
	dbGetQuery(con,sql)
	
	## Fill User Defined table
	cat("Fill",newTableName,"table\n")
	sql <- paste("INSERT INTO",newTableName,"SELECT NULL,",colName,"FROM probes_temp GROUP BY",colName)
	dbGetQuery(con,sql)
	
	## Fill Probes table
	cat("Fill Probes table\n")
	sql <- paste("INSERT INTO probes SELECT _id,probe_id FROM probes_temp p,",newTableName," s WHERE p.",colName," = s.",colName," GROUP BY p.probe_id",sep="")
	dbGetQuery(con,sql)
	
	## Fill meta Table
	cat("Fill meta Table\n")
	sql <- paste("INSERT INTO meta (key,value) VALUES ('main_table','",newTableName,"')")
	dbGetQuery(con,sql)
	
	## Fill table_master_meta Table
	cat("Fill table_master_meta Table\n")
	sql <- paste("INSERT INTO table_master_meta (tablename,fieldnames) VALUES ('probes','probe_id')")
	dbGetQuery(con,sql)
	sql <- paste("INSERT INTO table_master_meta (tablename,fieldnames) VALUES ('",newTableName,"','",colName,"')",sep="")
	dbGetQuery(con,sql)
	
	## SetIdLink
	setIdLink(con,newTableName,link)
	
	## Create index for main and probes table
	cat("Create index for main and probes table\n")
	sql <- paste("CREATE INDEX F",newTableName," ON ",newTableName,"(_id)",sep="")
	dbGetQuery(con,sql)	
	sql <- paste("CREATE INDEX FPROBES ON ",newTableName,"(_id)",sep="")
	dbGetQuery(con,sql)	
	
	## Remove Helper table
	sql <- "DROP TABLE IF EXISTS probes_temp"
	dbGetQuery(con,sql)	
	
	## Add metadata Table for interoperability with other functions
	cat("Add metadata Table\n")
	sql <- "CREATE TABLE metadata (name VARCHAR(40) PRIMARY KEY,value VARCHAR(80) NOT NULL)"
	dbGetQuery(con,sql)
	
	## Add the first Bimap if bimapName was defined
	addBimapObj(con,bimapName,'probes',newTableName)
		
	## Add map_counts Table for interoperability with other functions
	cat("Add map_counts Table\n")
	sql <- "CREATE TABLE map_counts (map_name VARCHAR(80) PRIMARY KEY,count INTEGER NOT NULL)"
	dbGetQuery(con,sql)
	
	sql <- "INSERT INTO map_counts SELECT 'TOTAL',COUNT(*) FROM probes"
	dbGetQuery(con,sql)
	
	sql <- paste("INSERT INTO map_counts SELECT '",bimapName,"',COUNT(*) FROM probes",sep="")
	dbGetQuery(con,sql)
	
	## DBSCHEMA and DBSCHEMAVERSION must be defined for compatibility
	sql <- paste("INSERT INTO metadata (name,value) VALUES ('DBSCHEMA','",species,"_dbX_schema')",sep="")
	dbGetQuery(con,sql)
	sql <- paste("INSERT INTO metadata (name,value) VALUES ('DBSCHEMAVERSION','1.0')")
	dbGetQuery(con,sql)
		
	return(TRUE)
})
