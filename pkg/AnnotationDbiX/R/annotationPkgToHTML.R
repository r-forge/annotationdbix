## x = SQLite Database oder AnnDbBimap Objekt
## caption = Tabellenüberschrift
## outputDir = dort wird das html File generiert
## tables = Tabellen, die verwendet werden. 1 Tabelle ist die Haupttabelle
## onlyIDs = wenn TRUE werden nur IDs angezeigt. Bei FALSE werden auch andere Attribute angezeigt
## extdata = zusätzliche Daten. Mindestens 2 Spalten. Die erste Spalte muss IDs der Haupttabelle enthalten. Jede weitere Spalte wird einzeln zur Haupttabelle gemappt (NAs und "" erlaubt). Header werden als Spaltenüberschriften und bei colOrder verwendet.
## colOrder = legt die Reihenfolge der Spalten fest. Bei tables werden die Tabellennamen verwendet bei extdata die Spaltenüberschriften
## css = Pfad zu css File, dass dann im html code eingebettet wird
## filter = SQL WHERE Statement, welche die Hauptabelle filtert

setGeneric("annotationPkgToHTML", signature = c("x","caption","outputDir","tables"),
	function(x,caption,outputDir,tables,onlyIDs=FALSE,extdata=NULL,colOrder=NULL,css="", ...) standardGeneric("annotationPkgToHTML"))
	
## FilePath
setMethod("annotationPkgToHTML", signature("character","character","character","character"),
function(x,caption,outputDir,tables,onlyIDs=FALSE,extdata=NULL,colOrder=NULL,css="") 
{
	## Check Parameters
	if(!file.exists(x))
		stop("Database do not exist!")
	
	## Load SQLite Driver
	drv <- dbDriver("SQLite")
	
	## Generate Connection Object	
	con <- dbConnect(drv, dbname = x)
	on.exit(dbDisconnect(con))
	
	annotationPkgToHTML(con,caption,outputDir,tables,onlyIDs,extdata,colOrder,css)
})

setMethod("annotationPkgToHTML", signature(x="AnnDbBimap",caption="character",outputDir="character",tables="missing"),
function(x,caption,outputDir,direction=1,onlyIDs=FALSE,extdata=NULL,colOrder=NULL,css="") 
{
	if(direction==1)
		tables = c(x@L2Rchain[[1]]@tablename,x@L2Rchain[[2]]@tablename)
	else if(direction==-1)
		tables = c(x@L2Rchain[[2]]@tablename,x@L2Rchain[[1]]@tablename)
	else
		stop("'direction' must be 1 or -1\n")
		
	f <- paste(x@L2Rchain[[1]]@filter,"AND",x@L2Rchain[[2]]@filter)
	
	#annotationPkgToHTML(x@datacache$dbconn,caption,outputDir,tables,onlyIDs,extdata,colOrder,css,f)
})

setMethod("annotationPkgToHTML",signature("SQLiteConnection","character","character","character"),
function(x,caption,outputDir,tables,onlyIDs=FALSE,extdata=NULL,colOrder=NULL,css="") 
{	
	con <- x
	
	## Read meta
	cat("Read table_master_meta\n")
	sql <- "SELECT * FROM table_master_meta"
	tableInfo <- dbGetQuery(con,sql)	
	mainCol <- apply(tableInfo[2],1,function(x) strsplit(x,";")[[1]][1])
	tableInfo <- as.data.frame(cbind(tableInfo,mainCol),stringsAsFactors=FALSE)
	
	if(length(tables) == 1 && tables == "*")
		tables <- tableInfo[['tablename']]
	
	## If onlyIDs TRUE then only ids where used without their attributes
	if(onlyIDs)
		fieldNames <- tableInfo[tableInfo[['tablename']] %in% tables,c('tablename','mainCol')] 
	else
		fieldNames <- tableInfo[tableInfo[['tablename']] %in% tables,c('tablename','fieldnames')]

	for(i in length(tables))
		if(!dbExistsTable(con,tables[i]))
			stop("Table '",tables[i],"' does not exist\n")
			
	if(!is.null(extdata))
		if(!class(extdata)=='data.frame')
			stop("'extdata' must be from class data.frame\n")
	
	results <- list()
	
	select1 <- paste(fieldNames[1,1],".",strsplit(fieldNames[1,2],";")[[1]],collapse=",",sep="")
	
	## Get all unique IDs from the main column
	sql <- paste("SELECT DISTINCT",select1,"FROM",fieldNames[1,1])
	print(sql)
	results[[1]] <- dbGetQuery(con,sql)
	print(results[[1]])
	
	## Get all unique IDs from the other columns
	for(i in 2:length(tables))
	{	
		select2 <- paste(fieldNames[i,1],".",strsplit(fieldNames[i,2],";")[[1]],collapse=",",sep="")
		
		sql <- paste("SELECT DISTINCT ",fieldNames[1,1],".",strsplit(fieldNames[1,2],";")[[1]][1],",",select2," FROM ",fieldNames[1,1]," LEFT OUTER JOIN ",fieldNames[i,1]," ON ",fieldNames[1,1],"._id = ",fieldNames[i,1],"._id",sep="")
		print(sql)
		results[[i]] <- dbGetQuery(con,sql)
	}
	
	html <- "<table border='1'>"
	
	## Writes Header
	print("Header")
	html <- paste(html,"<thead><tr><th>",colnames(results[[1]]),"</th>")
	print("Header2")
  	for(i in 2:length(results))
	{
		html <- paste(html,"<th>",colnames(results[[i]][-1]),"</th>",collapse="\n")
	}
	
	html <- paste(html,"</tr></thead><tbody>")
	print(seq(along=results[[1]]))	
	
	for(i in 1:nrow(results[[1]]))
	{
		html <- paste(html,"<tr><td>",results[[1]][i,1],"</td>")
		print("Body2")
		for(j in 2:length(results))
		{
			html <- paste(html,"<td>")
			print("Body3")
			for(k in 2:ncol(results[[j]]))
			{
				html <- paste(html,paste(results[[j]][results[[j]][,1] == results[[1]][i,1],k],collapse="<br/>"))
			}
			html <- paste(html,"</td>")
		}
		html <- paste(html,"</tr>")
	}	
	print("Body4")
	html <- paste(html,"</tbody></table>")
	
	return(html)

	#join <- paste("LEFT OUTER JOIN ",tables[-1]," ON ",tables[1],"._id = ",tables[2:length(tables)],"._id",sep="",collapse=" ")
		
	#SELECT * FROM probes a1 LEFT OUTER JOIN go_id a2 ON a1._id = a2._id LEFT OUTER JOIN kegg a3 ON a1._id = a3._id
	
	#sql<- paste("SELECT",select,"FROM",tables[1],join)
	#print(sql)
	#return(dbGetQuery(con,sql))
})
