## Add new Annotation Data to the .dbX database

## TODO: In Zukunft statt der probe_id die primer seq. verwenden, die ist 100% unique

## Load Library
#require("RSQLite")

setGeneric("addNewAnnotation2", function(x,data,tableName,data.colnames) standardGeneric("addNewAnnotation2"))

## FilePath
setMethod("addNewAnnotation2", signature("character","data.frame","character","character"), function(x,data,tableName,data.colnames) 
{
	## Check Parameter
	if(!file.exists(x))
		stop("Database do not exist!")
	
	## Load SQLite Driver
	drv <- dbDriver("SQLite")
	
	## Generate Connection Object	
	con <- dbConnect(drv, dbname = x)
	on.exit(dbDisconnect(con))
	
	addNewAnnotation(con,data,tableName,data.colnames)
})

## SQLite-Connection
setMethod("addNewAnnotation2", signature("SQLiteConnection","data.frame","character","character"), function(x,data,tableName,data.colnames) 
{	
	con <- x
	
	if(dbExistsTable(con,tableName))
		stop("Table '",tablename,"' already exists\n")
	
	## Read meta
	cat("Read table_master_meta\n")
	sql <- "SELECT * FROM table_master_meta"
	id_tables <- dbGetQuery(con,sql)	
	
	sql <- "SELECT * FROM meta"
	meta <- dbGetQuery(con,sql)
	main_table <- meta[meta$key == 'main_table','value']
	
	if(!(colnames(data)[1] %in% id_tables[1]))
		stop("There is no table named",colnames(data)[1])
	
	## Add helper table	
	cat("Add helper table ",tableName,"_temp\n",sep="")
	colnames(data) <- data.colnames
	dbWriteTable(conn=con,name=paste(tableName,"_temp",sep=""),value=data[data.colnames],row.names=FALSE,overwrite=TRUE)	
	
	## Create new table
	cat("Create new table",tableName,"\n")
	if(length(data.colnames[-1:-2]) != 0)
		dyn <- paste(",",paste(data.colnames[-1:-2],"TEXT",collapse=","))
	else
		dyn <- ""
		
	sql <- paste("CREATE TABLE",tableName,"(_id INTEGER REFERENCES ",main_table,"(_id) NOT NULL,",data.colnames[2]," TEXT NOT NULL",dyn,")")
	dbGetQuery(con,sql)
	
	## Fill new table
	#cat("Fill new table",tablename,"\n")
	#dyn <- paste(data.colnames[-1],collapse=",")
	#sql <- paste("INSERT INTO ",tablename," SELECT _id,",dyn," FROM ",tablename,"_temp t,probes_id p WHERE p.probe_id = t.probe_id AND p._id IS NOT NULL",sep="")
	#dbGetQuery(con,sql)
	
	## Update table_master_meta
	#cat("Update table_master_meta\n")
	#sql <- paste("INSERT INTO table_master_meta VALUES('",tableName,"','",paste(data.colnames[-1],collapse=";"),"')",sep="")

	#dbGetQuery(con,sql)
	
	## Remove helper table
	cat("Remove helper table\n")
	sql <- paste("DROP TABLE ",tablename,"_temp",sep="")
	dbGetQuery(con,sql)
})
