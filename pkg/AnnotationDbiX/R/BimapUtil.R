## Add BiMapObjects to the Database

setGeneric("addBimapObj", signature = c("x","name","table1","table2"), function(x,name,table1,table2,comment="Added by addBimapObj()",tagname1="",tagname2="",filter1="",filter2="") standardGeneric("addBimapObj"))

setGeneric("deleteBimapObj", signature = c("x","name"), function(x,name) standardGeneric("deleteBimapObj"))

setGeneric("listBimapObj", signature = c("x"), function(x) standardGeneric("listBimapObj"))

## FilePath
setMethod("addBimapObj", signature("character","character","character","character"), function(x,name,table1,table2,comment="Added by addBimapObj()",tagname1="",tagname2="",filter1="",filter2="") 
{
	## Check Parameter
	if(!file.exists(x))
		stop("Database do not exist!")
	
	## Load SQLite Driver
	drv <- dbDriver("SQLite")
	
	## Generate Connection Object	
	con <- dbConnect(drv, dbname = x)
	on.exit(dbDisconnect(con))
	
	addBimapObj(con,name,table1,table2,comment,tagname1,tagname2,filter1,filter2)
})


## DB-Connection
setMethod("addBimapObj", signature(x="SQLiteConnection",name="character",table1="character",table2="character"), function(x,name,table1,table2,comment="Added by addBimapObj()",tagname1="",tagname2="",filter1="",filter2="") 
{	
	con <- x;
	
	## Check Parameter
	if(!dbExistsTable(con,table1))
		stop("Table1 do not exist!")
		
	if(!dbExistsTable(con,table2))
		stop("Table2 do not exist!")
		
	## Create bimap_meta table if not exist
		
	sql <- "CREATE TABLE IF NOT EXISTS bimap_meta(name TEXT PRIMARY KEY,table1 TEXT NOT NULL,table2 TEXT NOT NULL,tagname1 TEXT,tagname2 TEXT,comment TEXT,filter1 TEXT,filter2 TEXT)"
	dbGetQuery(con,sql)
	
	## Check if Bimap Object already exists
	sql <- paste("SELECT COUNT(*) FROM bimap_meta WHERE name='",name,"'",sep="")
	if(dbGetQuery(con,sql) > 0)
	{
		cat("Bimap object '",name,"' already exist. Use deleteBimapObj() to delete bimap object.\n",sep="")
		return(FALSE)
	}
	
	## Add new Bimap Object
	sql <- paste("INSERT INTO bimap_meta VALUES ('",name,"','",table1,"','",table2,"','",tagname1,"','",tagname2,"','",comment,"','",filter1,"','",filter2,"')",sep="")
	dbGetQuery(con,sql)
	cat("Bimap Object '",name,"' added.\nDetach the library now and load it new to update existing bimap objects.\n",sep="")
	return(TRUE)
})


## Delete BiMapObjects to the Database


## FilePath
setMethod("deleteBimapObj", signature("character","character"), function(x,name) 
{
	## Check Parameter
	if(!file.exists(x))
		stop("Database do not exist!")
	
	## Load SQLite Driver
	drv <- dbDriver("SQLite")
	
	## Generate Connection Object	
	con <- dbConnect(drv, dbname = x)
	on.exit(dbDisconnect(con))
	
	deleteBimapObj(con,name)
})

setMethod("deleteBimapObj", signature("SQLiteConnection","character"), function(x,name) 
{	
	con <- x
	
	## Check if bimap_meta exists
	if(!dbExistsTable(con,'bimap_meta'))
	{
		cat("There are no bimap objects in database. Use addBimapObj() to add bimap objects.\n")
		return(FALSE)
	}	
		
	## Check if bimap object exists
	sql <- paste("SELECT COUNT(*) FROM bimap_meta WHERE name='",name,"'",sep="")
	if(dbGetQuery(con,sql) == 0)
	{
		cat("Can not delete '",name,"'. Bimap object '",name,"' does not exist.\n",sep="")
		return(FALSE)
	}
	
	## Delete Bimap Object
	sql <- paste("DELETE FROM bimap_meta WHERE name='",name,"'",sep="")
	dbGetQuery(con,sql)
	sql <- "SELECT COUNT(*) FROM bimap_meta"
	if(dbGetQuery(con,sql) == 0)
	{
		sql <- "DROP TABLE bimap_meta"
		dbGetQuery(con,sql)
	}
	
	cat("Bimap Object '",name,"' deleted.\nDetach the library now and load it new to update existing bimap objects.\n",sep="")
	return(TRUE)
})

## List all Bimap Objects
## FilePath
setMethod("listBimapObj", signature("character"), function(x) 
{
	## Check Parameter
	if(!file.exists(x))
		stop("Database do not exist!")
	
	## Load SQLite Driver
	drv <- dbDriver("SQLite")
	
	## Generate Connection Object	
	con <- dbConnect(drv, dbname = x)
	on.exit(dbDisconnect(con))
	
	listBimapObj(con)
})

## DB-Conn
setMethod("listBimapObj", signature("SQLiteConnection"), function(x) 
{
	con <- x
	
	## Check if bimap_meta exists
	if(!dbExistsTable(con,'bimap_meta'))
	{
		cat("There are no bimap objects in database. Use addBimapObj() to add bimap objects.\n")
		return(FALSE)
	}	
	
	## List bimap objects
	sql <- "SELECT * FROM bimap_meta"
	return(dbGetQuery(con,sql))
})
