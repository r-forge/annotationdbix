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
	function(x,caption,outputDir,mainTable,tables=character(),filter=character(),onlyIDs=FALSE,extdata=NULL,colOrder=NULL,css="",tableRows=50, ...) standardGeneric("annotationPkgToHTML"))
	
## FilePath
setMethod("annotationPkgToHTML", signature("character","character","character","character"),
function(x,caption,outputDir,mainTable,tables=character(),filter=character(),onlyIDs=FALSE,extdata=NULL,colOrder=NULL,css="",tableRows=50) 
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
function(x,caption,outputDir,tables=character(),filter=character(),direction=1,onlyIDs=FALSE,extdata=NULL,colOrder=NULL,css="",tableRows=50) 
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
function(x,caption,outputDir,mainTable,tables=character(),filter=character(),onlyIDs=FALSE,extdata=NULL,colOrder=NULL,css="",tableRows=50) 
{	
	con <- x
	
	path <- strsplit(outputDir,.Platform$file.sep)[[1]]
	print(fileName <- path[length(path)])
	path <- paste(path[1:(length(path)-1)],collapse=.Platform$file.sep)
	
	## Read meta
	cat("Read table_master_meta\n")
	sql <- "SELECT * FROM table_master_meta"
	tableInfo <- dbGetQuery(con,sql)	
	mainCol <- apply(tableInfo[2],1,function(x) strsplit(x,";")[[1]][1])
	
	tableInfo <- as.data.frame(cbind(tableInfo,mainCol,stringsAsFactors=FALSE),stringsAsFactors=FALSE)
	rownames(tableInfo) <- tableInfo[[1]]
	
	## All _id tables are included
	if(length(tables) == 0)
		tables <- tableInfo[['tablename']]
	else
		tables <- c(mainTable,tables)
			
	## Test if tables exist
	for(i in length(tables))
		if(!dbExistsTable(con,tables[i]))
			stop("Table '",tables[i],"' does not exist\n")
	
	## If onlyIDs TRUE then only ids where used without their attributes
	if(onlyIDs)
		tableInfo <-cbind(tableInfo[tables,c('tablename','mainCol','mainCol','links')],1:length(tables))
	else
		tableInfo <-cbind(tableInfo[tables,c('tablename','fieldnames','mainCol','links')],1:length(tables))
	
	nCols <- 0		

	if(!is.null(colOrder))
	{
		## colOrder must be unique
		if(length(colOrder)!=length(unique(colOrder)))
			stop("'colOrder' must be unique\n")
		
		## All column names must be in colOrder
		if(!all(tables %in% colOrder))
		{
			stop(paste("'",tables[!(tables %in% colOrder)],"'",sep="",collapse=",")," is not in 'colOrder'\n")
		}	
		nCols <- nCols + length(tables)
		
		
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
	
	## Test filter
	if(!is.character(filter))
		stop("'filter' must be of type character\n")
		
	## Set tableRows 50 if not valid
	if((!is.numeric(tableRows)) || tableRows < 1)
		tableRows <- 50
		
	## Get all unique IDs from the main column
	results <- list()
	
	select1 <- paste(mainTable,".",strsplit(tableInfo[mainTable,2],";"),collapse=",",sep="")
	
	sql <- paste("SELECT DISTINCT",select1,"FROM",mainTable)

	results[[1]] <- dbGetQuery(con,sql)
	results[[1]] <- cbind(results[[1]][,1],results[[1]])
	
	## Filter mainTable rows
	if(length(filter) > 0)
	{
		results[[1]] <- results[[1]][results[[1]][,1] %in% filter,]
	}
	
	## Get all unique IDs from the other columns
	for(i in 2:nrow(tableInfo))
	{		
		select2 <- paste(tableInfo[i,1],".",strsplit(tableInfo[i,2],";")[[1]],collapse=",",sep="")
		
		sql <- paste("SELECT DISTINCT ",mainTable,".",tableInfo[mainTable,3],",",select2," FROM ",mainTable," LEFT OUTER JOIN ",tableInfo[i,1]," ON ",mainTable,"._id = ",tableInfo[i,1],"._id ORDER BY ",mainTable,".",tableInfo[mainTable,3],sep="")
		
		results[[i]] <- dbGetQuery(con,sql)
	}
	
	## Add extdata columns to results
	if(!is.null(extdata))
	{
		extdata <- merge(results[[1]][1],extdata,all.x=TRUE,by.x=1,by.y=1)
		
		for(i in 2:ncol(extdata))
		{
			results[[length(results) + 1]] <- extdata[c(1,i)] 
			temp <- data.frame(colnames(extdata[i]),colnames(extdata[i]),colnames(extdata[i]),"",tableInfo[nrow(tableInfo),5] + 1,row.names=colnames(extdata[i]))
			colnames(temp) <- colnames(tableInfo)
			
			tableInfo <- rbind(tableInfo,temp)
		}
	}
	
	#print(system.time({

	numPages <- ceiling(nrow(results[[1]])/tableRows)
	## Write HTML pages
	for(p in 1:numPages)
	{
		## Generate HTML side
		cat("Write page ",p," of ",numPages,"\n")
		
		## Write DOCTYPE and META
		html <- paste('<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">',
					  '<html xmlns="http://www.w3.org/1999/xhtml">',
					  '<head>',
					  '<meta http-equiv="Content-Type" content="text/html; charset=utf-8" />',
					  '<title>',caption,'</title>',
					  sep="\n")
		
		if(is.null(css))
			html <- paste(html,'</head>','<body>',sep="\n")			  
		else if(css=="")
        	html <- paste(html,
        			'<style type="text/css">
        			/* >![CDATA[ */
					table {
						border-width: medium medium medium medium;
						border-spacing: 2px;
						border-style: outset outset outset outset;
						border-color: blue blue blue blue;
						border-collapse: collapse;
						background-color: #fae0d6;
						width: 100%;
					}
					table caption{
						font-size: 1cm;
						font-style: italic;
						font-weight: bold;
						font-variant: small-caps;
					}
					table th {
						white-space:nowrap;
						font-size: 0.6cm;
						border-width: 0px 2px 2px 2px;
						padding: 5px 10px 5px 10px;
						border-style: solid solid solid solid;
						border-color: blue blue blue blue;
						-moz-border-radius: 0px 0px 0px 0px;
					}
					table td {
						border-width: 0px 2px 0px 2px;
						padding: 2px 10px 2px 10px;
						border-style: solid solid solid solid;
						border-color: white blue white blue;
						vertical-align:text-top;
						-moz-border-radius: 0px 0px 0px 0px;
					}
					.even {
						background-color: #fdfdfd;	
					}
					.odd {
						background-color: #dddddd;	
					}
					table tr:hover {
						background-color: #f6f5b2;
					}
					.links {
						width: 100%;
						text-align:center; 
						font-size: 0.7cm;
					}/* ]] */
					</style>','</head>','<body>',
        	sep="\n")
        else if(is.character(css))
        	html <- paste(html,paste('<link rel="stylesheet" type="text/css" href="',css,'" />',sep=""),'</head>','<body>',sep="\n")
        else
        	stop("'css' must be from type character or NULL.")
        
        ## Writes links to the other sides
        if(numPages == 1)
        	html <- paste(html,"<div class='links'><p>", p,"/",numPages,"</p></div>",sep="")
        else if(p == numPages)
        	html <- paste(html,"<div class='links'><p><a href='",fileName,p-1,".html'>&lt;</a>", p,"/",numPages,"</p></div>",sep="")
        else if(p == 1)
        	html <- paste(html,"<div class='links'><p>", p,"/",numPages,"<a href='",fileName,p+1,".html'>&gt;</a></p></div>",sep="")
        else
	        html <- paste(html,"<div class='links'><p><a href='",fileName,p-1,".html'>&lt;</a>", p,"/",numPages,"<a href='",fileName,p+1,".html'>&gt;</a></p></div>",sep="")
	        
		if(is.null(css))        	
			html <- paste(html,"<table border='1'><caption>",sep="")
		else
			html <- paste(html,"<table><caption>",caption,"</caption>",sep="")
		
	    	
		## Write table header
		header <- ""	
		for(i in 1:length(results))
			header <- paste(header,paste("<th>",colnames(results[[tableInfo[colOrder[i],5]]][-1]),"</th>",collapse="",sep=""),sep="")
		
		html <- paste(html,"<thead><tr>",header,"</tr></thead><tbody>")
		
		## Write table body
		from <- (p-1)*tableRows+1
		to <- 0
		
		if(p == numPages)
			to <- nrow(results[[1]])
		else
			to <- p*tableRows
			
		body<-lapply(results[[1]][from:to,1],function(x)

		{	
			v <- c()
			for(i in 1:length(results))
			{
				for(j in 2:ncol(results[[tableInfo[colOrder[i],5]]]))
				{
					if(any(is.na(res<-results[[tableInfo[colOrder[i],5]]][results[[tableInfo[colOrder[i],5]]][,1] == x,j])))
						v<-c(v,"&nbsp;")
					else if(tableInfo[colOrder[i],4] == "") 
						v<-c(v,paste(res,collapse="<br />",sep=""))
					else
					{
						links <- sapply(res,function(x) sub("\\$ID",x,tableInfo[colOrder[i],4]))
						v<-c(v,paste("<a href='",links,"'>",res,"</a>",collapse="<br />",sep=""))
					}
				}
			}
			paste("<td>",v,"</td>",collapse="",sep="")
		})
		
		html <- paste(html,paste("<tr class=",c("'even'","'odd'"),">",body,"</tr>",collapse="\n",sep=""),"</tbody></table>",sep="")
	
		## Writes links to the other sides
        if(numPages == 1)
        	html <- paste(html,"<div class='links'><p>", p,"/",numPages,"</p></div></body></html>",sep="")
        else if(p == numPages)
        	html <- paste(html,"<div class='links'><p><a href='",fileName,p-1,".html'>&lt;</a>", p,"/",numPages,"</p></div></body></html>",sep="")
        else if(p == 1)
        	html <- paste(html,"<div class='links'><p>", p,"/",numPages,"<a href='",fileName,p+1,".html'>&gt;</a></p></div></body></html>",sep="")
        else
	        html <- paste(html,"<div class='links'><p><a href='",fileName,p-1,".html'>&lt;</a>", p,"/",numPages,"<a href='",fileName,p+1,".html'>&gt;</a></p></div></body></html>",sep="")
	        
	        
		## Write File
		cat(file=paste(outputDir,p,".html",sep=""),html)
	}

	#}))
})
