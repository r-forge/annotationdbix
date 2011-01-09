setGeneric("addBimapObj", signature = c("x","name","table1","table2"), function(x,name,table1,table2,comment="Added by addBimapObj()",tagname1="",tagname2="",filter1="",filter2="",revmap=FALSE) standardGeneric("addBimapObj"))

setGeneric("addSimpleBimapObj", signature = c("x","name","table"), function(x,name,table,comment="Added by addBimapObj()",tagname="",filter="",revmap=FALSE) standardGeneric("addSimpleBimapObj"))

setGeneric("deleteBimapObj", signature = c("x","name"), function(x,name) standardGeneric("deleteBimapObj"))

setGeneric("listBimapObj", signature = c("x"), function(x) standardGeneric("listBimapObj"))

setGeneric("setFilterBimapObj", signature = c("x","name","bimap","filter"), function(x,name,bimap,filter,comment="Added by setFilterBimapObj()") standardGeneric("setFilterBimapObj"))

setGeneric("setIdLink", signature = c("x","table","link"), function(x,table,link) standardGeneric("setIdLink"))

## Add BiMapObjects to the Database

## FilePath
setMethod("addBimapObj", signature("character","character","character","character"), function(x,name,table1,table2,comment="Added by addBimapObj()",tagname1,tagname2,filter1,filter2,revmap=FALSE) 
{
	## Check Parameter
	if(!file.exists(x))
		stop("Database does not exist!\n")
	
	## Load SQLite Driver
	drv <- dbDriver("SQLite")
	
	## Generate Connection Object	
	con <- dbConnect(drv, dbname = x)
	on.exit(dbDisconnect(con))
	
	addBimapObj(con,name,table1,table2,comment,tagname1,tagname2,filter1,filter2,revmap)
})


## DB-Connection
setMethod("addBimapObj", signature(x="SQLiteConnection",name="character",table1="character",table2="character"), function(x,name,table1,table2,comment="Added by addBimapObj()",tagname1,tagname2,filter1,filter2,revmap=FALSE) 
{	
	con <- x;
	
	## Check Parameter
	if(!dbExistsTable(con,table1))
		stop("Table1 does not exist!\n")
		
	if(!dbExistsTable(con,table2))
		stop("Table2 does not exist!\n")
		
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
		cat("Bimap object '",name,"' already exist. Use deleteBimapObj() to delete a Bimap object.\n",sep="")
		return(FALSE)
	}
	
	if(tagname1[1] != "")
	{
		for(i in seq(along=tagname1))
		{
			if(is.null(names(tagname1[i])) || names(tagname1[i]) == "")
				tagname1[i] <- paste(tagname1[i],"=",tagname1[i],sep="")
			else
				tagname1[i] <- paste(names(tagname1[i]),"=",tagname1[i],sep="")
		}
		tagname1 <- paste(tagname1,collapse=";",sep="")
	}
	
	if(tagname2[1] != "")
	{
		for(i in seq(along=tagname2))
		{
			if(is.null(names(tagname2[i])) || names(tagname2[i]) == "")
				tagname2[i] <- paste(tagname2[i],"=",tagname2[i],sep="")
			else
				tagname2[i] <- paste(names(tagname2[i]),"=",tagname2[i],sep="")
		}	
		tagname2 <- paste(tagname2,collapse=";",sep="")
	}
	
	filter1 <- gsub("'",'"',filter1)
	filter2 <- gsub("'",'"',filter2)
	
	## Add new Bimap Object
	sql <- paste("INSERT INTO bimap_meta VALUES ('",name,"','",table1,"','",table2,"','",tagname1,"','",tagname2,"','",comment,"','",filter1,"','",filter2,"','",revmap,"')",sep="")
	
	dbGetQuery(con,sql)
	
	cat("Bimap object '",name,"' added.\nTo update the new created Bimap objects detach and reload the library.\n",sep="")
	
	return(TRUE)
})

## FilePath
setMethod("addSimpleBimapObj", signature("character","character","character"), function(x,name,table,comment="Added by addBimapObj()",tagname,filter,revmap=FALSE) 
{
	## Check Parameter
	if(!file.exists(x))
		stop("Database does not exist!\n")
	
	## Load SQLite Driver
	drv <- dbDriver("SQLite")
	
	## Generate Connection Object	
	con <- dbConnect(drv, dbname = x)
	on.exit(dbDisconnect(con))
	
	addBimapObj(con,name,table,comment,tagname,filter,revmap)
})


## DB-Connection
setMethod("addSimpleBimapObj", signature(x="SQLiteConnection",name="character",table="character"), function(x,name,table,comment="Added by addBimapObj()",tagname,filter,revmap=FALSE) 
{	
	con <- x;
	
	## Check Parameter
	if(!dbExistsTable(con,table))
		stop("Table does not exist!\n")
		
		
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
		cat("Bimap object '",name,"' already exist. Use deleteBimapObj() to delete a Bimap object.\n",sep="")
		return(FALSE)
	}
	
	if(tagname[1] != "")
	{
		for(i in seq(along=tagname))
		{
			if(is.null(names(tagname[i])) || names(tagname[i]) == "")
				tagname[i] <- paste(tagname[i],"=",tagname[i],sep="")
			else
				tagname[i] <- paste(names(tagname[i]),"=",tagname[i],sep="")
		}
		tagname <- paste(tagname,collapse=";",sep="")
	}
	
	
	filter <- gsub("'",'"',filter)
	
	## Add new Bimap Object
	sql <- paste("INSERT INTO bimap_meta VALUES ('",name,"','",table,"','','",tagname,"','','",comment,"','",filter,"','','",revmap,"')",sep="")
	
	dbGetQuery(con,sql)
	
	cat("Simple Bimap object '",name,"' added.\nTo update the new created Bimap objects detach and reload the library.\n",sep="")
	
	return(TRUE)
})

## Delete BiMapObjects to the Database

## FilePath
setMethod("deleteBimapObj", signature("character","character"), function(x,name) 
{
	## Check Parameter
	if(!file.exists(x))
		stop("Database does not exist!\n")
	
	## Load SQLite Driver
	drv <- dbDriver("SQLite")
	
	## Generate Connection Object	
	con <- dbConnect(drv, dbname = x)
	on.exit(dbDisconnect(con))
	
	deleteBimapObj(con,name)
})

## AnnDbBimap Object
setMethod("deleteBimapObj", signature("AnnDbBimap","missing"), function(x) 
{	
	con <- x@datacache$dbconn
	name <- x@objName
	
	deleteBimapObj(con,name)
})

setMethod("deleteBimapObj", signature("SQLiteConnection","character"), function(x,name) 
{	
	con <- x
	
	## Check if bimap_meta exists
	if(!dbExistsTable(con,'bimap_meta'))
	{
		cat("There are no Bimap objects in the database. Use addBimapObj() to add Bimap objects.\n")
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
	
	cat("Bimap object '",name,"' was deleted.\nDetach the library now and load it new to update all existing Bimap objects.\n",sep="")
	return(TRUE)
})



## List all Bimap Objects
## FilePath
setMethod("listBimapObj", signature("character"), function(x) 
{
	## Check Parameter
	if(!file.exists(x))
		stop("Database does not exist!\n")
	
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
		cat("There are no Bimap objects in the database. Use addBimapObj() to add new Bimap objects.\n")
		return(FALSE)
	}	
	
	## List bimap objects
	sql <- "SELECT * FROM bimap_meta"
	return(dbGetQuery(con,sql))
})

## Add special filtered BiMapObjects to the Database
## Currently broken !!!
## FilePath
#setMethod("setFilterBimapObj", signature(x="character",name="character",bimap="AnnDbBimap",filter="character"), function(x,name,bimap,filter,comment="Added by filterBimapObj()") 
#{
	## Check Parameter
#	if(!file.exists(x))
#		stop("Database does not exist!\n")
	
	## Load SQLite Driver
#	drv <- dbDriver("SQLite")
	
	## Generate Connection Object	
#	con <- dbConnect(drv, dbname = x)
#	on.exit(dbDisconnect(con))
	
#	setFilterBimapObj(con,name,bimap,filter,comment)
#})


## DB-Connection
#setMethod("setFilterBimapObj", signature(x="SQLiteConnection",name="character",bimap="AnnDbBimap",filter="character"), function(x,name,bimap,filter,comment="Added by setFilterBimapObj()") 
#{	
#	con <- x;
	
	## Create bimap_meta table if not exist		
#	sql <- "CREATE TABLE IF NOT EXISTS bimap_meta(name TEXT PRIMARY KEY,table1 TEXT NOT NULL,table2 TEXT NOT NULL,tagname1 TEXT,tagname2 TEXT,comment TEXT,filter1 TEXT,filter2 TEXT)"
#	dbGetQuery(con,sql)
	
	## Check if Bimap Object already exists
#	sql <- paste("SELECT COUNT(*) FROM bimap_meta WHERE name='",name,"'",sep="")
#	if(dbGetQuery(con,sql) > 0)
#	{
#		cat("Bimap object '",name,"' already exist. Use deleteBimapObj() to delete a Bimap object.\n",sep="")
#		return(FALSE)
#	}
	
	## Get tableinfo
#	sql <- "SELECT * FROM table_master_meta"
#	tableinfo <- dbGetQuery(con,sql)
#	tableinfo <- as.data.frame(cbind(tableinfo,apply(tableinfo[2],1,function(x) strsplit(x,";")[[1]][1])),stringsAsFactors=FALSE)
#	mainCol <- tableinfo[tableinfo['tablename']==bimap@L2Rchain[[2]]@tablename,4]
	
	## Add new Bimap Object
#	if(is.null(bimap@L2Rchain[[1]]@tagname) || is.na(bimap@L2Rchain[[1]]@tagname))
#		tagname1 = ""
#	else
#		tagname1 = bimap@L2Rchain[[1]]@tagname
#		
#	if(is.null(bimap@L2Rchain[[2]]@tagname) || is.na(bimap@L2Rchain[[2]]@tagname))
#		tagname2 = ""
#	else
#		tagname2 = bimap@L2Rchain[[2]]@tagname
#	
#	filter2 <- paste(bimap@L2Rchain[[2]]@filter,'AND',paste("{",mainCol,"}=\"",filter,"\"",sep="",collapse=" OR "))
#
#	return(addBimapObj(con,name,bimap@L2Rchain[[1]]@tablename,bimap@L2Rchain[[2]]@tablename,comment,tagname1=tagname1,tagname2=tagname2,filter1=bimap@L2Rchain[[1]]@filter,filter2=filter2))
#})

## Set the HTML link in the .dbX database associating to a biodatabase which is used in the html as Id link generated by the annotationPkgToHTML function.

## FilePath
setMethod("setIdLink", signature("character","character","character"), function(x,table,link) 
{
	## Check Parameter
	if(!file.exists(x))
		stop("Database does not exist!\n")
	
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
	sql <- paste("SELECT * FROM table_master_meta WHERE tablename = '",table,"'",sep="")
	result <- dbGetQuery(con,sql)
	
	## Check if table is in table_master_meta
	if(table != result[[1]][1])
		stop("Table '",table,"' is not listed in 'table_master_meta'.\n")		
	
	if(length(link) != length(strsplit(result[[2]],";")[[1]]))
	
	for(i in 1:length(strsplit(result[[2]],";")[[1]]))
		if(is.na(link[i]))
			link[i] <- ""
	
	## Concatenate all links to one string
	link <- paste(link,collapse="|")
		
	## Mask all & with the correct &amp; for valid html
	link <- paste(strsplit(link,"&(?!amp)",perl=TRUE)[[1]],collapse="&amp;",sep="")
	
	sql <- paste("UPDATE table_master_meta SET links = '",link,"' WHERE tablename = '",table,"'",sep="")
	dbGetQuery(con,sql)
	
	return(TRUE)
})
