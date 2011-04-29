## Add new Annotation Data to the .dbX database
setGeneric("addNewAnnotation", signature = c("x","data","newTableName","data.colNames","mapTableName"),function(x,data,newTableName,data.colNames,mapTableName,tableTypeLength,bimapName) standardGeneric("addNewAnnotation"))

setGeneric("addNewSimpleAnnotation", signature = c("x","data","newTableName","data.colNames"),function(x,data,newTableName,data.colNames,tableTypeLength,bimapName) standardGeneric("addNewSimpleAnnotation"))

setGeneric("addNewAnnotationFromDb1", signature = c("x","data","mapTableName","mapDb1TableName","dbSrc"),function(x,data,mapTableName,mapDb1TableName,dbSrc) standardGeneric("addNewAnnotationFromDb1"))

## FilePath
setMethod("addNewAnnotation", signature("character","data.frame","character","character","character"), function(x,data,newTableName,data.colNames,mapTableName,tableTypeLength,bimapName) 
{
	## Check Parameters
	if(!file.exists(x))
		stop("Database does not exist!\n")
	
	## Load SQLite Driver
	drv <- dbDriver("SQLite")
	
	## Generate Connection Object	
	con <- dbConnect(drv, dbname = x)
	on.exit(dbDisconnect(con))
	
	addNewAnnotation(con,data,newTableName,data.colNames,mapTableName,tableTypeLength,bimapName)
})

## SQLite-Connection
setMethod("addNewAnnotation", signature("SQLiteConnection","data.frame","character","character","character"), function(x,data,newTableName,data.colNames,mapTableName,tableTypeLength,bimapName) 
{	
	con <- x
	
	## Check if tableTypeLength is integer
	if(!missing(tableTypeLength))
		if(!isTRUE(all.equal(as.integer(tableTypeLength), tableTypeLength)))
			stop("'tableTypeLength' must be from type 'integer'.\n")

	## Check bimapName
	if(!missing(bimapName))
		if(is.character(bimapName))
		{
			if(length(bimapName) > 1)
				stop(paste("length(",bimapName,") must be 1.\n",sep=""))
		}
		else
			stop(paste("'",bimapName,"' must be from type 'character'.\n",sep=""))
			
	## Check if table already exists
	if(dbExistsTable(con,newTableName))
		stop("Table '",newTableName,"' already exists.\n")
	
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
	
	data <- unique(data[,1:(length(data.colNames)+1)])
	colnames(data) <- c(as.character(tableInfo[tableInfo$tablename == mapTableName,'mainCol'][1]),data.colNames)
	
	## Check maximum length of data rows
	max.colLength <- apply(data[-1],2,function(x) max(nchar(x)))
	
	if(missing(tableTypeLength))
		tableTypeLength <- max.colLength
		
	## Test if vector length are equal
	if(length(tableTypeLength) != length(max.colLength))
		stop("Vector length of 'tableTypeLength' must be as long as ncol(data)-1.\n")
		
	## Test if length of data rows <= 
	if(!all(tableTypeLength >= max.colLength))
		stop("There are data entries longer than in 'tableTypeLength' set.\n")
				
	## Add helper table	
	if(length(ind<-which(data[[2]] == "")) != 0)
		data[ind,2] <- NA
		
	cat("Add helper table ",newTableName,"_temp\n",sep="")
	dbWriteTable(conn=con,name=paste(newTableName,"_temp",sep=""),value=data,row.names=FALSE,overwrite=TRUE)
	
	## Create new table
	cat("Create new table",newTableName,"\n")
	if(length(data.colNames[-1]) != 0)
		dyn <- paste(",",paste(data.colNames[-1],"VARCHAR(",tableTypeLength[-1],")",collapse=","))
	else
		dyn <- ""
		
	sql <- paste("CREATE TABLE ",newTableName," (_id INTEGER NOT NULL,",colnames(data)[2]," VARCHAR(",tableTypeLength[1],") NOT NULL",dyn,",FOREIGN KEY (_id) REFERENCES ",main_table," (_id))",sep="")
	dbGetQuery(con,sql)
	
	## Fill new table
	cat("Fill new table",newTableName,"\n")
	dyn <- paste("t.",data.colNames,collapse=",",sep="")
	sql <- paste("INSERT INTO ",newTableName," SELECT p._id,",dyn," FROM ",newTableName,"_temp t,",mapTableName," p WHERE p.",colnames(data)[1]," = t.",colnames(data)[1]," AND t.",colnames(data)[2]," IS NOT NULL",sep="")
	dbGetQuery(con,sql)
	
	## Create index for main and probes table
	cat(paste("Create index for '",newTableName,"'\n",sep=""))
	sql <- paste("CREATE INDEX F",newTableName," ON ",newTableName,"(_id)",sep="")
	dbGetQuery(con,sql)	
	
	## Add Bimap if bimapName was defined
	if(!missing(bimapName))
		addBimapObj(con,bimapName,'probes',newTableName)
	
	## Update table_master_meta
	cat("Update table_master_meta\n")
	sql <- paste("INSERT INTO table_master_meta VALUES('",newTableName,"','",paste(data.colNames,collapse=";"),"','",paste(rep('|',length(data.colNames)-1),collapse=''),"')",sep="")
	dbGetQuery(con,sql)
	
	## Remove helper table
	cat("Remove helper table\n")
	sql <- paste("DROP TABLE ",newTableName,"_temp",sep="")
	dbGetQuery(con,sql)
	
	return(TRUE)
})

## FilePath
setMethod("addNewSimpleAnnotation", signature("character","data.frame","character","character"), function(x,data,newTableName,data.colNames,tableTypeLength,bimapName) 
{
	## Check Parameters
	if(!file.exists(x))
		stop("Database does not exist!\n")
	
	## Load SQLite Driver
	drv <- dbDriver("SQLite")
	
	## Generate Connection Object	
	con <- dbConnect(drv, dbname = x)
	on.exit(dbDisconnect(con))
	
	addNewSimpleAnnotation(con,data,newTableName,data.colNames,tableTypeLength,bimapName)
})

## SQLite-Connection
setMethod("addNewSimpleAnnotation", signature("SQLiteConnection","data.frame","character","character"), function(x,data,newTableName,data.colNames,tableTypeLength,bimapName) 
{	
	con <- x
	
	## Check if tableTypeLength is integer
	if(!missing(tableTypeLength))
		if(!isTRUE(all.equal(as.integer(tableTypeLength), tableTypeLength)))
			stop("'tableTypeLength' must be from type 'integer'.\n")

	## Check bimapName
	if(!missing(bimapName))
		if(is.character(bimapName))
		{
			if(length(bimapName) > 1)
				stop(paste("length(",bimapName,") must be 1.\n",sep=""))
		}
		else
			stop(paste("'",bimapName,"' must be from type 'character'.\n",sep=""))
			
	## Check if table already exists
	if(dbExistsTable(con,newTableName))
		stop("Table '",newTableName,"' already exists.\n")
	
	## Read meta
	cat("Read table_master_meta\n")
	sql <- "SELECT * FROM table_master_meta"
	tableInfo <- dbGetQuery(con,sql)	
	mainCol <- apply(tableInfo[2],1,function(x) strsplit(x,";")[[1]][1])
	tableInfo <- as.data.frame(cbind(tableInfo,mainCol,stringsAsFactors=FALSE),stringsAsFactors=FALSE)
	
	sql <- "SELECT * FROM meta"
	meta <- dbGetQuery(con,sql)
	main_table <- meta[meta$key == 'main_table','value']
	
	colnames(data) <- data.colNames
	
	## Check maximum length of data rows
	max.colLength <- apply(data,2,function(x) max(nchar(x)))
	
	if(missing(tableTypeLength))
		tableTypeLength <- max.colLength
		
	## Test if vector length are equal
	if(length(tableTypeLength) != length(max.colLength))
		stop("Vector length of 'tableTypeLength' must be as long as ncol(data)-1.\n")
		
	## Test if length of data rows <= 
	if(!all(tableTypeLength >= max.colLength))
		stop("There are data entries longer than in 'tableTypeLength' set.\n")
				
		
	cat("Add helper table ",newTableName,"_temp\n",sep="")
	dbWriteTable(conn=con,name=paste(newTableName,"_temp",sep=""),value=data,row.names=FALSE,overwrite=TRUE)
	
	## Create new table
	cat("Create new table",newTableName,"\n")
	
	dyn <- paste(data.colNames," VARCHAR(",tableTypeLength,")",collapse=",",sep="")
		
	sql <- paste("CREATE TABLE ",newTableName," (",dyn,")",sep="")

	dbGetQuery(con,sql)
	
	## Fill new table
	cat("Fill new table",newTableName,"\n")

	sql <- paste("INSERT INTO ",newTableName," SELECT * FROM ",newTableName,"_temp",sep="")
	dbGetQuery(con,sql)
	
	## Create index for main and probes table
	cat(paste("Create index for '",newTableName,"'\n",sep=""))
	sql <- paste("CREATE INDEX F",newTableName," ON ",newTableName,"(",data.colNames,")",sep="")
	dbGetQuery(con,sql)	
	
	## Add Bimap if bimapName was defined

	if(!missing(bimapName))
		addSimpleBimapObj(con,bimapName,newTableName)
	
	## Update table_master_meta
	cat("Update table_master_meta\n")
	sql <- paste("INSERT INTO table_master_meta VALUES('",newTableName,"','",paste(data.colNames,collapse=";"),"','",paste(rep('|',length(data.colNames)-1),collapse=''),"')",sep="")
	dbGetQuery(con,sql)
	
	## Remove helper table
	cat("Remove helper table\n")
	sql <- paste("DROP TABLE ",newTableName,"_temp",sep="")
	dbGetQuery(con,sql)

	return(TRUE)
})

## FilePath 
setMethod("addNewAnnotationFromDb1", signature("character","data.frame","character","character","ANY"), function(x,data,mapTableName,mapDb1TableName,dbSrc) 
{
	## Check Parameters
	if(!file.exists(x))
		stop("Database does not exist!\n")
	
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
		stop("There is no table named '",mapTableName,"' in the extended .db database.\n")
	}
		
	## Get tableinfo from db1
	sql <- "SELECT * FROM db1.table_master_meta"
	tableInfoDb1 <- dbGetQuery(con,sql)
	mainColDb1 <- apply(tableInfoDb1[2],1,function(x) strsplit(x,";")[[1]][1])
	tableInfoDb1 <- as.data.frame(cbind(tableInfoDb1,mainColDb1,stringsAsFactors=FALSE),stringsAsFactors=FALSE)
	
	if(!(mapDb1TableName %in% tableInfoDb1[[1]]))
	{
		.detach_db(con)	
		stop("There is no table named '",mapDb1TableName,"' in the .db1 database.\n")
	}	
	
	## Add helper table	
	cat("Add helper table data_temp\n")
	colnames(data) <- c("V1","V2")
	dbWriteTable(conn=con,name="data_temp",value=unique(data),row.names=FALSE,overwrite=TRUE)	
	
	dbBeginTransaction(con)
	
	for(i in 1:nrow(tableInfoDb1))
	{
		cat("Create new table '",tableInfoDb1[i,1],"'\n",sep="")
		
		if(dbExistsTable(con,tableInfoDb1[i,1]))
		{
			dbRollback(con)
			.detach_db(con)
			stop("Table '",tableInfoDb1[i,1],"' already exists in the extended .db database.\n")
		}
		else
		{
			tryCatch(dbGetQuery(con,tableInfoDb1[i,3]),error=function(e) 
			{ 
				dbRollback(con)
				.detach_db(con)
				stop("Cannot create table '",tableInfoDb1[i,3],"'.\n") 
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
				stop("Cannot insert data into '",tableInfoDb1[i,1],"'.\n") 
			})
		}
		
		## Fill meta Table
		cat("Fill meta Table.\n")
		sql <- paste("INSERT INTO table_master_meta (tablename,fieldnames,links) VALUES ('",tableInfoDb1[i,1],"','",tableInfoDb1[i,2],"','",tableInfoDb1[i,4],"')",sep="")
		dbGetQuery(con,sql)
		
		## Create index for each table
		cat(paste("Create index for '",tableInfoDb1[i,1],"'.\n",sep=""))
		sql <- paste("CREATE INDEX F",tableInfoDb1[i,1]," ON ",tableInfoDb1[i,1],"(_id)",sep="")
		dbGetQuery(con,sql)		
	}
	
	# Create bimap_meta table
	sql <- "CREATE TABLE IF NOT EXISTS bimap_meta(name TEXT PRIMARY KEY,table1 TEXT NOT NULL,table2 TEXT NOT NULL,tagname1 TEXT,tagname2 TEXT,comment TEXT,filter1 TEXT,filter2 TEXT,revmap INTEGER)"
	dbGetQuery(con,sql)
			
	# Update bimap_meta table with all bimap objects from the .db1 database except bimap objects
	# which have the same name in the .dbX bimap_meta table
	cat("Update bimap_meta table.\n")
	sql <- "INSERT INTO bimap_meta SELECT * FROM db1.bimap_meta WHERE name IN (SELECT name FROM db1.bimap_meta except SELECT name FROM bimap_meta)"
	dbGetQuery(con,sql)	
			
	tryCatch(dbGetQuery(con,sql),error=function(e) 
	{ 
		dbRollback(con)
		.detach_db(con)
		stop("Cannot insert data into '",tableInfoDb1[i,1],"'.\n") 
	})
	
	if (!dbCommit(con))
	{
    	dbRollback(con)
    	stop("Commit failed.\n")
	}
	
	## Copy metadata table	
	cat("Add meta table.\n")
	sql <- "SELECT * FROM metadata"
	metadata1 <- dbGetQuery(con,sql)
	
	## Copy metadata table db1
	cat("Add meta table.\n")
	sql <- "SELECT * FROM db1.metadata"
	metadata2 <- dbGetQuery(con,sql)
	
	metadata<- metadata2[!(metadata2[[1]] %in% metadata1[[1]]),]
	
	for(i in 1:nrow(metadata))
	{
		sql <- paste("INSERT INTO metadata VALUES('",metadata[i,1],"','",metadata[i,2],"')",sep="")
		dbGetQuery(con,sql)
	}
	
	## Remove helper table
	cat("Remove helper table.\n")
	sql <- paste("DROP TABLE data_temp",sep="")
	dbGetQuery(con,sql)
	
	.detach_db(con)
	
	return(TRUE)
})

.attach_db <- function(con,dbconn)
{
	## Attach Database
	cat('Attach database',dbGetInfo(dbconn)$dbname,'as db1.\n')
	sql <- paste("ATTACH '",dbGetInfo(dbconn)$dbname,"' AS db1",sep="")
	dbGetQuery(con,sql)
}

.detach_db <- function(dbconn)
{
	## Detach Database
	cat('Detach database',dbGetInfo(dbconn)$dbname,'.\n')
	sql <- "DETACH db1"
	dbGetQuery(dbconn,sql)
}
