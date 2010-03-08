## x = SQLite Database oder AnnDbBimap Objekt
## caption = Tabellenüberschrift
## outputDir = dort wird das html File generiert
## tables = Tabellen, die verwendet werden. 1 Tabelle ist die Haupttabelle
## onlyIDs = wenn TRUE werden nur IDs angezeigt. Bei FALSE werden auch andere Attribute angezeigt
## extdata = zusätzliche Daten. Mindestens 2 Spalten. Die erste Spalte muss IDs der Haupttabelle enthalten. Jede weitere Spalte wird einzeln zur Haupttabelle gemappt (NAs und "" erlaubt). Header werden als Spaltenüberschriften und bei colOrder verwendet.
## colOrder = legt die Reihenfolge der Spalten fest. Bei tables werden die Tabellennamen verwendet bei extdata die Spaltenüberschriften
## css = Pfad zu css File, dass dann im html code eingebettet wird
## filter = SQL WHERE Statement, welche die Hauptabelle filtert


# TODO: HTML auf mehrere Seiten aufteilen

setGeneric("annotationPkgToHTML", signature = c("x","caption","outputDir","mainTable"),
	function(x,caption,outputDir,mainTable,tables=c(),onlyIDs=FALSE,extdata=NULL,colOrder=NULL,css="", ...) standardGeneric("annotationPkgToHTML"))
	
## FilePath
setMethod("annotationPkgToHTML", signature("character","character","character","character"),
function(x,caption,outputDir,mainTable,tables=c(),onlyIDs=FALSE,extdata=NULL,colOrder=NULL,css="") 
{
	## Check Parameters
	if(!file.exists(x))
		stop("Database do not exist!")
	
	## Load SQLite Driver
	drv <- dbDriver("SQLite")
	
	## Generate Connection Object	
	con <- dbConnect(drv, dbname = x)
	on.exit(dbDisconnect(con))
	
	annotationPkgToHTML(con,caption,outputDir,mainTable,tables,onlyIDs,extdata,colOrder,css)
})

setMethod("annotationPkgToHTML", signature(x="AnnDbBimap",caption="character",outputDir="character",mainTable="missing"),
function(x,caption,outputDir,tables=NULL,direction=1,onlyIDs=FALSE,extdata=NULL,colOrder=NULL,css="") 
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
function(x,caption,outputDir,mainTable,tables=c(),onlyIDs=FALSE,extdata=NULL,colOrder=NULL,css="") 
{	
	con <- x
	
	## Read meta
	cat("Read table_master_meta\n")
	sql <- "SELECT * FROM table_master_meta"
	tableInfo <- dbGetQuery(con,sql)	
	mainCol <- apply(tableInfo[2],1,function(x) strsplit(x,";")[[1]][1])
	
	tableInfo <- as.data.frame(cbind(tableInfo,mainCol,stringsAsFactors=FALSE),stringsAsFactors=FALSE)
	
	if(length(tables) == 1 && tables == "*")
		tables <- tableInfo[['tablename']]
	
	if(!dbExistsTable(con,mainTable))
			stop("Main table '",mainTable,"' does not exist\n")
			
	for(i in length(tables))
		if(!dbExistsTable(con,tables[i]))
			stop("Table '",tables[i],"' does not exist\n")
	
	nCols <- 1		

	if(!is.null(colOrder))
	{
		## colOrder must be unique
		if(length(colOrder)!=length(unique(colOrder)))
			stop("'colOrder' must be unique\n")
		
		## All table names must be in colOrder
		if(!is.null(tables))
		{
			if(!all(tables %in% colOrder))
			{
				stop("'",tables[tables %in% colOrder],"' is not in 'colOrder'\n")
			}	
			nCols <- nCols + length(tables)
		}
		
		## All extdata header names must be in colOrder
		if(!is.null(extdata))
		{
			if(!all(colnames(extdata[-1]) %in% colOrder))
			{
				stop("'",colnames(extdata[-1])[!(colnames(extdata[-1]) %in% colOrder)],"' is not in 'colOrder'\n")
			}
			nCols <- nCols + ncol(extdata[-1])
		}
		
		if(length(colOrder) != nCols)
			stop("'colOrder' must have a length of ",nCols,"\n")
	}	

	## If onlyIDs TRUE then only ids where used without their attributes
	if(onlyIDs)
	{
		fieldNames <- tableInfo[tableInfo[['tablename']] %in% tables,c('tablename','mainCol','links')] 
		fieldNames <- rbind(tableInfo[tableInfo[['tablename']] == mainTable,c('tablename','mainCol','links')],fieldNames)
	}
	else
	{
		fieldNames <- tableInfo[tableInfo[['tablename']] %in% tables,c('tablename','fieldnames','links')]
		fieldNames <- rbind(tableInfo[tableInfo[['tablename']] == mainTable,c('tablename','fieldnames','links')],fieldNames)		
	}
		
	mainTableInfo <- tableInfo[tableInfo[['tablename']] == mainTable,]

	# Sort fieldNames like tables is sort
	rownames(fieldNames) <- fieldNames[[1]]
	fieldNames <- cbind(fieldNames[c(mainTable,tables),],index=c(1:nrow(fieldNames)))
	
	if(!is.null(extdata))
		if(!is.data.frame(extdata))
			stop("'extdata' must be from class data.frame\n")
	
	results <- list()
	
	select1 <- paste(mainTable,".",strsplit(mainTableInfo[[2]],";"),collapse=",",sep="")
	links <- c(mainTableInfo[3])

	## Get all unique IDs from the main column
	sql <- paste("SELECT DISTINCT",select1,"FROM",mainTable)

	results[[1]] <- dbGetQuery(con,sql)
	results[[1]] <- cbind(results[[1]][1],results[[1]])
	
	
	## Get all unique IDs from the other columns
	for(i in 2:nrow(fieldNames))
	{		
		select2 <- paste(fieldNames[i,1],".",strsplit(fieldNames[i,2],";")[[1]],collapse=",",sep="")
		
		sql <- paste("SELECT DISTINCT ",mainTable,".",mainTableInfo[4],",",select2," FROM ",mainTable," LEFT OUTER JOIN ",fieldNames[i,1]," ON ",mainTable,"._id = ",fieldNames[i,1],"._id",sep="")
print(sql)
		results[[i]] <- dbGetQuery(con,sql)
		links[i] <- fieldNames[i,3] 
		
	}
print(3)
	if(!is.null(extdata))
	{
		extdata <- merge(results[[1]][1],extdata,all.x=TRUE,by.x=1,by.y=1)
		
		for(i in 2:ncol(extdata))
		{
			results[[length(results) + 1]] <- extdata[c(1,i)] 
			fieldNames <- rbind(fieldNames,data.frame(colnames(extdata[i]),"","",fieldNames[nrow(fieldNames),4] + 1))
			
		}
	}
	rownames(fieldNames) <- fieldNames[[1]]
	
	print(fieldNames[-3])
	print(system.time({
	
	## Generate HTML side
	cat("Generate the HTML file\n")
	html <- paste("<table border='1'><caption>",caption,"</caption>",sep="")
	
	## Write Header
	header <- paste("<th>",colnames(results[[1]]),"</th>",collapse="",sep="")
	
	for(i in 2:length(results))
	{
		header <- paste(header,paste("<th>",colnames(results[[i]][-1]),"</th>",collapse=""))
	}
	
	html <- paste(html,"<thead><tr>",header,"</tr></thead><tbody>")
	
	
	b <-c()
	print(length(results))
	## Write Body
	l<-lapply(results[[1]][,1],function(x)
	{	
		# Main Column 
		#mainResult <- results[[1]][results[[1]] == x,][1]
		#if(mainTableInfo[[3]] == "")
		#	v <- paste(mainResult,collapse="<br/>",sep="")
		#else
		#{
		#	links <- sapply(mainResult,function(x) sub("\\$ID",x,link))
		#	v<-c(v,paste("<a href='",links,"'>",res,"</a>",collapse="<br/>",sep=""))
		#}
		
		v <- c()
		
		# Other Columns
		for(i in 1:length(results))
		{
			link <- fieldNames[i,3]

			print(fieldNames[i,4])
			for(j in 2:ncol(results[[fieldNames[i,4]]]))
			{
				print("START")
				if(any(is.na(res<-results[[fieldNames[i,4]]][results[[fieldNames[i,4]]][,1] == x,j])))
					v<-c(v,"&nbsp;")
				else if(is.na(link) || link=="") # if extdata is set link is NA for this columns
					v<-c(v,paste(res,collapse="<br/>",sep=""))
				else
				{
					links <- sapply(res,function(x) sub("\\$ID",x,link))
					v<-c(v,paste("<a href='",links,"'>",res,"</a>",collapse="<br/>",sep=""))
				}
				print("END")
			}
		}
		paste("<td>",v,"</td>",collapse="",sep="")
	})
	
	html <- paste(html,paste("<tr>",l,"</tr>",collapse="\n",sep=""),"</tbody></table>",sep="")
	
	## Write File
	cat(file=outputDir,html)

	})) # system.time()
	
	

	#join <- paste("LEFT OUTER JOIN ",tables[-1]," ON ",tables[1],"._id = ",tables[2:length(tables)],"._id",sep="",collapse=" ")
		
	#SELECT * FROM probes a1 LEFT OUTER JOIN go_id a2 ON a1._id = a2._id LEFT OUTER JOIN kegg a3 ON a1._id = a3._id
	
	#sql<- paste("SELECT",select,"FROM",tables[1],join)
	#print(sql)
	#return(dbGetQuery(con,sql))
})
