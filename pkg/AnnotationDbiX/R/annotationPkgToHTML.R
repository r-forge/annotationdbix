setGeneric("annotationPkgToHTML", signature = c("x","mainTable","caption"),
	function(x,mainTable,caption,tables="",onlyIDs=FALSE,extdata=NULL,colOrder=NULL,css="") standardGeneric("annotationPkgToHTML"))
	
## FilePath
setMethod("annotationPkgToHTML", signature("character","character","character"),function(x,mainTable,caption,tables="",onlyIDs=FALSE,extdata=NULL,colOrder=NULL,css="") 
{
	## Check Parameters
	if(!file.exists(x))
		stop("Database do not exist!")
	
	## Load SQLite Driver
	drv <- dbDriver("SQLite")
	
	## Generate Connection Object	
	con <- dbConnect(drv, dbname = x)
	on.exit(dbDisconnect(con))
	
	annotationPkgToHTML(con,mainTable,caption,tables,onlyIDs,extdata,colOrder,css)
})

setMethod("annotationPkgToHTML", signature("AnnDbBimap","character","character"),function(x,mainTable,caption,tables="",onlyIDs=FALSE,extdata=NULL,colOrder=NULL,css="") 
{
	if(tables=="")
		tables = c(x@L2Rchain[[1]]@tablename,x@L2Rchain[[2]]@tablename)
		
	annotationPkgToHTML(x@datacache$dbconn,mainTable,caption,tables,onlyIDs,extdata,colOrder,css)
})

setMethod("annotationPkgToHTML",signature("SQLiteConnection","character","character"),
function(x,mainTable,caption,tables="",onlyIDs=FALSE,extdata=NULL,colOrder=NULL,css="") 
{	
	con <- x
	
	for(i in length(tables))
		if(!dbExistsTable(con,tables[i]))
			stop("Table '",tables[i],"' does not exist\n")
			
	if(!dbExistsTable(con,mainTable))
			stop("Table '",mainTable,"' does not exist\n")
			
	if(!is.null(extdata))
		if(!class(extdata)=='data.frame')
			stop("'extdata' must be from class data.frame\n")
			
	## Read meta
	cat("Read table_master_meta\n")
	sql <- "SELECT * FROM table_master_meta"
	tableInfo <- dbGetQuery(con,sql)	
	mainCol <- apply(tableInfo[2],1,function(x) strsplit(x,";")[[1]][1])
	tableInfo <- as.data.frame(cbind(tableInfo,mainCol),stringsAsFactors=FALSE)
	
	print(tableInfo)
})
