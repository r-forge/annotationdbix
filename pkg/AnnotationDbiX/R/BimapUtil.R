

setGeneric("addBimapObj", signature = c("x","name","table1","table2"), function(x,name,table1,table2,comment="Added by addBimapObj()",tagname1="",tagname2="",filter1="",filter2="",revmap=FALSE) standardGeneric("addBimapObj"))

setGeneric("deleteBimapObj", signature = c("x","name"), function(x,name) standardGeneric("deleteBimapObj"))

setGeneric("listBimapObj", signature = c("x"), function(x) standardGeneric("listBimapObj"))

setGeneric("setfilterBimapObj", signature = c("x","name","bimap","filter"), function(x,name,bimap,filter,comment="Added by filterBimapObj()") standardGeneric("setfilterBimapObj"))

setGeneric("setIdLink", signature = c("x","table","link"), function(x,table,link) standardGeneric("setIdLink"))

## Add BiMapObjects to the Database

## FilePath
setMethod("addBimapObj", signature("character","character","character","character"), function(x,name,table1,table2,comment="Added by addBimapObj()",tagname1="",tagname2="",filter1="",filter2="",revmap=FALSE) 
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
setMethod("addBimapObj", signature(x="SQLiteConnection",name="character",table1="character",table2="character"), function(x,name,table1,table2,comment="Added by addBimapObj()",tagname1="",tagname2="",filter1="",filter2="",revmap=FALSE) 
{	
	con <- x;
	
	## Check Parameter
	if(!dbExistsTable(con,table1))
		stop("Table1 do not exist!")
		
	if(!dbExistsTable(con,table2))
		stop("Table2 do not exist!")
		
	## Create bimap_meta table if not exist
	sql <- "CREATE TABLE IF NOT EXISTS bimap_meta(name TEXT PRIMARY KEY,table1 TEXT NOT NULL,table2 TEXT NOT NULL,tagname1 TEXT,tagname2 TEXT,comment TEXT,filter1 TEXT,filter2 TEXT,revmap INTEGER)"
	
	dbGetQuery(con,sql)
	
	## If revmap TRUE @direction = -1 otherwise @direction = 1
	if(revmap)
		revmap <- -1
	else
		revmap <- 1
		
	## Check if Bimap Object already exists
	sql <- paste("SELECT COUNT(*) FROM bimap_meta WHERE name='",name,"'",sep="")
	if(dbGetQuery(con,sql) > 0)
	{
		cat("Bimap object '",name,"' already exist. Use deleteBimapObj() to delete bimap object.\n",sep="")
		return(FALSE)
	}
	
	## Add new Bimap Object
	sql <- paste("INSERT INTO bimap_meta VALUES ('",name,"','",table1,"','",table2,"','",tagname1,"','",tagname2,"','",comment,"','",filter1,"','",filter2,"','",revmap,"')",sep="")
	dbGetQuery(con,sql)
	cat("Bimap Object '",name,"' added.\nTo update the bimaps objects detach and reload the library .\n",sep="")
	
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


## Add special filtered BiMapObjects to the Database

## FilePath
setMethod("setfilterBimapObj", signature(x="character",name="character",bimap="AnnDbBimap",filter="character"), function(x,name,bimap,filter,comment="Added by filterBimapObj()") 
{
	## Check Parameter
	if(!file.exists(x))
		stop("Database do not exist!")
	
	## Load SQLite Driver
	drv <- dbDriver("SQLite")
	
	## Generate Connection Object	
	con <- dbConnect(drv, dbname = x)
	on.exit(dbDisconnect(con))
	
	setfilterBimapObj(con,name,bimap,filter,comment)
})


## DB-Connection
setMethod("setfilterBimapObj", signature(x="SQLiteConnection",name="character",bimap="AnnDbBimap",filter="character"), function(x,name,bimap,filter,comment="Added by fliterBimapObj()") 
{	
	con <- x;
	
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
	
	## Get tableinfo
	sql <- "SELECT * FROM table_master_meta"
	tableinfo <- dbGetQuery(con,sql)
	tableinfo <- as.data.frame(cbind(tableinfo,apply(tableinfo[2],1,function(x) strsplit(x,";")[[1]][1])),stringsAsFactors=FALSE)
	mainCol <- tableinfo[tableinfo['tablename']==bimap@L2Rchain[[2]]@tablename,4]
	
	## Add new Bimap Object
	if(is.null(bimap@L2Rchain[[1]]@tagname) || is.na(bimap@L2Rchain[[1]]@tagname))
		tagname1 = ""
	else
		tagname1 = bimap@L2Rchain[[1]]@tagname
		
	if(is.null(bimap@L2Rchain[[2]]@tagname) || is.na(bimap@L2Rchain[[2]]@tagname))
		tagname2 = ""
	else
		tagname2 = bimap@L2Rchain[[2]]@tagname
	
	filter2 <- paste(bimap@L2Rchain[[2]]@filter,'AND',paste("{",mainCol,"}=\"",filter,"\"",sep="",collapse=" OR "))

	return(addBimapObj(con,name,bimap@L2Rchain[[1]]@tablename,bimap@L2Rchain[[2]]@tablename,comment,tagname1=tagname1,tagname2=tagname2,filter1=bimap@L2Rchain[[1]]@filter,filter2=filter2))
})

## Set the HTML link in the .dbX database associating to a biodatabase which is used in the html as Id link generated by the annotationPkgToHTML function.

## FilePath
setMethod("setIdLink", signature("character","character","character"), function(x,table,link) 
{
	## Check Parameter
	if(!file.exists(x))
		stop("Database do not exist!")
	
	## Load SQLite Driver
	drv <- dbDriver("SQLite")
	
	## Generate Connection Object	
	con <- dbConnect(drv, dbname = x)
	on.exit(dbDisconnect(con))
	
	setIdLink(con,table,link)
})

## DB-Connection
setMethod("setIdLink", signature("SQLiteConnection","character","character"), function(x,table,link) 
{	
	con <- x;
	
	## Check if bimap_meta exists
	if(!dbExistsTable(con,"table_master_meta"))
		stop("Table 'table_master_meta' does not exist.\n")

	## Check if table exists
	if(!dbExistsTable(con,table))
		stop("Table '",table,"' does not exist.\n")		
	
	## Get tableinfo
	sql <- "SELECT * FROM table_master_meta"
	result <- dbGetQuery(con,sql)
	
	## Check if table is in table_master_meta
	if(!(table %in% result[[1]]))
		stop("Table '",table,"' is not listed in 'table_master_meta'.\n")		
	
	## Check if $ID is set in the link
	if(length(grep("\\$ID",link)) == 0)
		stop("There is no $ID Tag in the link. This tag will be replaced with the ids from the .dbX database\n")	
		
	## Mask all & with the correct &amp; for valid html
	link <- paste(strsplit(link,"&(?!amp)",perl=TRUE)[[1]],collapse="&amp;",sep="")
	
	sql <- paste("UPDATE table_master_meta SET links = '",link,"' WHERE tablename = '",table,"'",sep="")
	dbGetQuery(con,sql)
})
