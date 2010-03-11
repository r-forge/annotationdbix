## Add new Annotation Data to the .dbX database

## TODO: In Zukunft statt der probe_id die primer seq. verwenden, die ist 100% unique

## Load Library
#require("RSQLite")

# x					dbConn() - Objekt oder Pfad
# data				Liste mit neuen AnnotationDaten 1.Spalte Ids die im Package bereits vorhanden sind; 2 Spalte neue Ids
# newTableName		Neuer Tablename
# data.colNames		Spalten für die neue Tabelle ohne _id
# mapTableName		Name der Table auf die gematched wird
# dbSrc				.db1 Datenbank Pfad oder dbConn() Objekt

setGeneric("addNewAnnotation2", signature = c("x","data","newTableName","data.colNames","mapTableName"),function(x,data,newTableName,data.colNames,mapTableName) standardGeneric("addNewAnnotation2"))

setGeneric("addNewAnnotationFromDb1", signature = c("x","data","mapTableName","mapDb1TableName","dbSrc"),function(x,data,mapTableName,mapDb1TableName,dbSrc) standardGeneric("addNewAnnotationFromDb1"))

## FilePath
setMethod("addNewAnnotation2", signature("character","data.frame","character","character","character"), function(x,data,newTableName,data.colNames,mapTableName) 
{
	## Check Parameters
	if(!file.exists(x))
		stop("Database do not exist!")
	
	## Load SQLite Driver
	drv <- dbDriver("SQLite")
	
	## Generate Connection Object	
	con <- dbConnect(drv, dbname = x)
	on.exit(dbDisconnect(con))
	
	addNewAnnotation2(con,data,newTableName,data.colNames,mapTableName)
})

## SQLite-Connection
setMethod("addNewAnnotation2", signature("SQLiteConnection","data.frame","character","character","character"), function(x,data,newTableName,data.colNames,mapTableName) 
{	
	con <- x
	
	if(dbExistsTable(con,newTableName))
		stop("Table '",newTableName,"' already exists\n")
	
	## Read meta
	cat("Read table_master_meta\n")
	sql <- "SELECT * FROM table_master_meta"
	tableInfo <- dbGetQuery(con,sql)	
	mainCol <- apply(tableInfo[2],1,function(x) strsplit(x,";")[[1]][1])
	tableInfo <- as.data.frame(cbind(tableInfo,mainCol,stringsAsFactors=FALSE),stringsAsFactors=FALSE)
	
	sql <- "SELECT * FROM meta"
	meta <- dbGetQuery(con,sql)
	main_table <- meta[meta$key == 'main_table','value']
	
	
	if(!(mapTableName %in% tableInfo[[1]]))
	stop("There is no table named ",mapTableName)

	colnames(data) <- c(as.character(tableInfo[tableInfo$tablename == mapTableName,'mainCol'][1]),data.colNames)
	
	## Add helper table	
	cat("Add helper table ",newTableName,"_temp\n",sep="")
	dbWriteTable(conn=con,name=paste(newTableName,"_temp",sep=""),value=unique(data[colnames(data)]),row.names=FALSE,overwrite=TRUE)	
	
	## Create new table
	cat("Create new table",newTableName,"\n")
	if(length(data.colNames[-1]) != 0)
		dyn <- paste(",",paste(data.colNames[-1],"TEXT",collapse=","))
	else
		dyn <- ""
		
	sql <- paste("CREATE TABLE",newTableName,"(_id INTEGER REFERENCES ",main_table,"(_id) NOT NULL,",colnames(data)[2]," TEXT NOT NULL",dyn,")")
	dbGetQuery(con,sql)
	
	## Fill new table
	cat("Fill new table",newTableName,"\n")
	dyn <- paste(data.colNames,collapse=",")
	sql <- paste("INSERT INTO ",newTableName," SELECT _id,",dyn," FROM ",newTableName,"_temp t,",mapTableName," p WHERE p.",colnames(data)[1]," = t.",colnames(data)[1]," AND p._id",sep="")
	dbGetQuery(con,sql)
	
	## Create index for main and probes table
	cat(paste("Create index for '",newTableName,"'\n",sep=""))
	sql <- paste("CREATE INDEX F",newTableName," ON ",newTableName,"(_id)",sep="")
	dbGetQuery(con,sql)	
	
	## Update table_master_meta
	cat("Update table_master_meta\n")
	sql <- paste("INSERT INTO table_master_meta VALUES('",newTableName,"','",paste(data.colNames,collapse=";"),"','')",sep="")
	dbGetQuery(con,sql)
	
	## Remove helper table
	cat("Remove helper table\n")
	sql <- paste("DROP TABLE ",newTableName,"_temp",sep="")
	dbGetQuery(con,sql)
})

## FilePath 
setMethod("addNewAnnotationFromDb1", signature("character","data.frame","character","character","ANY"), function(x,data,mapTableName,mapDb1TableName,dbSrc) 
{
	## Check Parameters
	if(!file.exists(x))
		stop("Database do not exist!")
	
	## Load SQLite Driver
	drv <- dbDriver("SQLite")
	
	## Generate Connection Object	
	con <- dbConnect(drv, dbname = x)
	on.exit(dbDisconnect(con))
	
	addNewAnnotationFromDb1(con,data,mapTableName,mapDb1TableName,dbSrc)
})

## SQLite-Connection
setMethod("addNewAnnotationFromDb1", signature("SQLiteConnection","data.frame","character","character","character"), function(x,data,mapTableName,mapDb1TableName,dbSrc) 
{	
	con <- x
	
	if(!file.exists(dbSrc))
		stop("dbScr '",dbSrc,"' does not exist!")	
	
	## Load SQLite Driver
	drv <- dbDriver("SQLite")
	
	## Generate Connection Object	
	con2 <- dbConnect(drv, dbname = dbSrc)
	on.exit(dbDisconnect(con2))
	
	addNewAnnotationFromDb1(con,data,mapTableName,mapDb1TableName,con2)
})

setMethod("addNewAnnotationFromDb1", signature("SQLiteConnection","data.frame","character","character","SQLiteConnection"), function(x,data,mapTableName,mapDb1TableName,dbSrc) 
{
	con <- x
		
	## Get tableinfo from dbX
	sql <- "SELECT * FROM table_master_meta"
	tableInfo <- dbGetQuery(con,sql)
	mainCol <- apply(tableInfo[2],1,function(x) strsplit(x,";")[[1]][1])
	tableInfo <- as.data.frame(cbind(tableInfo,mainCol,stringsAsFactors=FALSE),stringsAsFactors=FALSE)
	
	.attach_db(con,dbSrc)
	
	if(!(mapTableName %in% tableInfo[[1]]))
	{
		.detach_db(con)		
		stop("There is no table named '",mapTableName,"' in the dbX database")
	}
		
	## Get tableinfo from db1
	sql <- "SELECT * FROM db1.table_master_meta"
	tableInfoDb1 <- dbGetQuery(con,sql)
	mainColDb1 <- apply(tableInfoDb1[2],1,function(x) strsplit(x,";")[[1]][1])
	tableInfoDb1 <- as.data.frame(cbind(tableInfoDb1,mainColDb1,stringsAsFactors=FALSE),stringsAsFactors=FALSE)
	
	if(!(mapDb1TableName %in% tableInfoDb1[[1]]))
	{
		.detach_db(con)	
		stop("There is no table named '",mapDb1TableName,"' in the db1 database")
	}	
	
	## Add helper table	
	cat("Add helper table data_temp\n")
	dbWriteTable(conn=con,name="data_temp",value=unique(data),row.names=FALSE,overwrite=TRUE)	
	
	dbBeginTransaction(con)
	
	#print(tableInfoDb1)
	for(i in 1:nrow(tableInfoDb1))
	{
		cat("Create new table '",tableInfoDb1[i,1],"'\n",sep="")
		
		if(dbExistsTable(con,tableInfoDb1[i,1]))
		{
			dbRollback(con)
			.detach_db(con)
			stop("Table '",tableInfoDb1[i,1],"' already exists in the dbX database\n")
		}
		else
		{
			tryCatch(dbGetQuery(con,tableInfoDb1[i,3]),error=function(e) 
			{ 
				dbRollback(con)
				.detach_db(con)
				stop("Cannot create table '",tableInfoDb1[i,3],"'\n") 
			})
			
			fieldNames <- strsplit(tableInfoDb1[i,2],";")
			
			if(length(fieldNames[[1]]) > 1)
				dyn <- paste(paste("b.",fieldNames[[1]],sep=""),collapse=",")
			else
				dyn <- paste("b.",tableInfoDb1[i,5])	
			
			sql <- paste("INSERT INTO ",tableInfoDb1[i,1]," SELECT m._id, ",dyn," FROM db1.",mapDb1TableName," a,db1.",tableInfoDb1[i,1]," b,data_temp d,",mapTableName," m WHERE m.",tableInfo[tableInfo$tablename==mapTableName,4]," = d.V1 AND a.",tableInfoDb1[tableInfoDb1$tablename==mapDb1TableName,5]," = d.V2 AND b._id == a._id",sep="")

			tryCatch(dbGetQuery(con,sql),error=function(e) 
			{ 
				dbRollback(con)
				.detach_db(con)
				stop("Cannot insert data into '",tableInfoDb1[i,1],"'\n") 
			})
		}
		
		## Fill meta Table
		cat("Fill meta Table\n")
		sql <- paste("INSERT INTO table_master_meta (tablename,fieldnames,links) VALUES ('",tableInfoDb1[i,1],"','",tableInfoDb1[i,2],"','",tableInfoDb1[i,4],"')",sep="")
		dbGetQuery(con,sql)
		
		## Create index for each table
		cat(paste("Create index for '",tableInfoDb1[i,1],"'\n",sep=""))
		sql <- paste("CREATE INDEX F",tableInfoDb1[i,1]," ON ",tableInfoDb1[i,1],"(_id)",sep="")
		dbGetQuery(con,sql)		
	}
	
	# Create bimap_meta table
	sql <- "CREATE TABLE IF NOT EXISTS bimap_meta(name TEXT PRIMARY KEY,table1 TEXT NOT NULL,table2 TEXT NOT NULL,tagname1 TEXT,tagname2 TEXT,comment TEXT,filter1 TEXT,filter2 TEXT)"
	dbGetQuery(con,sql)
			
	# Update bimap_meta table with all bimap objects from the .db1 database except bimap objects
	# which have the same name in the .dbX bimap_meta table
	cat("Update bimap_meta table\n")
	sql <- "INSERT INTO bimap_meta SELECT * FROM db1.bimap_meta WHERE name IN (SELECT name FROM db1.bimap_meta except SELECT name FROM bimap_meta)"
	dbGetQuery(con,sql)	
			
	tryCatch(dbGetQuery(con,sql),error=function(e) 
	{ 
		dbRollback(con)
		.detach_db(con)
		stop("Cannot insert data into '",tableInfoDb1[i,1],"'\n") 
	})
	
	if (!dbCommit(con))
	{
    	dbRollback(con)
    	stop("Commit failed")
	}
	
	## Remove helper table
	cat("Remove helper table\n")
	sql <- paste("DROP TABLE data_temp",sep="")
	dbGetQuery(con,sql)
	
	.detach_db(con)
})

.attach_db <- function(con,dbconn)
{
	## Attach Database
	cat('Attach Database',dbGetInfo(dbconn)$dbname,'as db1\n')
	sql <- paste("ATTACH '",dbGetInfo(dbconn)$dbname,"' AS db1",sep="")
	dbGetQuery(con,sql)
}

.detach_db <- function(dbconn)
{
	## Detach Database
	cat('Detach Database',dbGetInfo(dbconn)$dbname,'\n')
	sql <- "DETACH db1"
	dbGetQuery(dbconn,sql)
}
