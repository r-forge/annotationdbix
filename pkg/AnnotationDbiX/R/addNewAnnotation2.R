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
	id_tables <- dbGetQuery(con,sql)	
	mainCol <- apply(id_tables[2],1,function(x) strsplit(x,";")[[1]][1])
	id_tables <- as.data.frame(cbind(id_tables,mainCol),stringsAsFactors=FALSE)
	
	sql <- "SELECT * FROM meta"
	meta <- dbGetQuery(con,sql)
	main_table <- meta[meta$key == 'main_table','value']
	
	
	if(!(mapTableName %in% id_tables[[1]]))
	stop("There is no table named ",mapTableName)

	colnames(data) <- c(as.character(id_tables[id_tables$tablename == mapTableName,'mainCol'][1]),data.colNames)
	
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
	
	## Update table_master_meta
	cat("Update table_master_meta\n")
	sql <- paste("INSERT INTO table_master_meta VALUES('",newTableName,"','",paste(data.colNames,collapse=";"),"')",sep="")
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
	con2 <- dbConnect(drv, dbname = x)
	on.exit(dbDisconnect(con2))
	
	addNewAnnotationFromDb1(con,data,mapTableName,mapDb1TableName,con2)
})

setMethod("addNewAnnotationFromDb1", signature("SQLiteConnection","data.frame","character","character","SQLiteConnection"), function(x,data,mapTableName,mapDb1TableName,dbSrc) 
{
	con <- x
	
	## Attach Database
	cat('Attach Database',dbSrc,'as db1\n')
	sql <- paste("ATTACH '",dbSrc,"' AS db1",sep="")
	dbGetQuery(con,sql)
		
	## Get colinfo
	sql <- paste("PRAGMA table_info(",data.colNames[1],")",sep="")
	print(sql)
	colinfo <- dbGetQuery(con,sql)
	toDrop <- colinfo[-1,'name']
	print(toDrop)
			
	if(!(mapTableName %in% id_tables[[1]]))
	stop("There is no table named ",mapTableName)
			
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
})
