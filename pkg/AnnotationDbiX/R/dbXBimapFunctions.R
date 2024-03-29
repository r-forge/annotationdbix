## Database schema function
.dbSchema <- function(x, show.indices=FALSE,tableName="")
{
	con <- NULL
	
	if(is.character(x))
	{
		if(file.exists(x))
		{
			tryCatch(
			{
				## Load SQLite Driver
				drv <- dbDriver("SQLite")
	
				## Generate Connection Object	
				con <- dbConnect(drv, dbname = x)
				on.exit(dbDisconnect(con))
			},
			error=function(e)
			{ 			
				stop("Cannot open database '",x,"'!\n") 
			})
		}
		else
			stop("File '",x,"' does not exist!\n") 
	}
	else if(class(x) == "SQLiteConnection")
	{
		con <- x
	}
	else if(class(x) == "AnnDbBimap")
	{
		con <- x@datacache$dbconn
	}
	
	## Check if tableName exists
	if(tableName != '' && !dbExistsTable(con,tableName))
	{
		cat(paste("Table '",tableName,"' does not exist'.\n",sep=""))
		return(NULL)
	}
	
	if(tableName=="")
		where <- "1"
	else
		where <- paste("tbl_name = '",tableName,"'",sep="")
		
	if(show.indices)
		sql <- paste("SELECT sql AS schema FROM sqlite_master WHERE (type = 'table' OR type ='index') AND",where)
	else
		sql <- paste("SELECT sql AS schema FROM sqlite_master WHERE type = 'table' AND",where)
	
	return(dbGetQuery(con,sql))
}

## Create bimap objects dyn. from database
.createBimapObjs <- function(dbconn,datacache,env)
{	
	## Check if bimap_meta exists
	if(!dbExistsTable(dbconn,'bimap_meta'))
	{
		cat('No Bimap objects available.\n')
		return(NULL)
	}
	
	## Get bimap objects
	sql <- "SELECT * FROM bimap_meta"
	bimaps <- dbGetQuery(dbconn,sql)

	## Get tableinfo
	sql <- "SELECT * FROM table_master_meta"
	tableinfo <- dbGetQuery(dbconn,sql)
	fields <- apply(tableinfo['fieldnames'],1,function(x) strsplit(x,";")[[1]][1])
	mainCol <- apply(tableinfo['fieldnames'],1,function(x) strsplit(x,";")[[1]][1])
	tableinfo <- as.data.frame(cbind(tableinfo,mainCol,stringsAsFactors=FALSE),stringsAsFactors=FALSE)
	MapCounts <- c()


	for(i in 1 : nrow(bimaps))
	{	
		tryCatch(
		{
			## Creates tagnames in bimap object if available
			tags1 <- NULL
			tags2 <- NULL
			tagnames <- NULL
			filter1 <- "1"
			filter2 <- "1"		
		
			if(bimaps[i,'tagname1'] != "")
			{
				tagnames <- strsplit(bimaps[i,'tagname1'],";")
	
				for(j in seq(along=tagnames))
				{
					key_value <- strsplit(tagnames[[1]][j],"=")		
					temp <- names(tags1)
					tags1 <- c(tags1,key_value[[1]][2])
					names(tags1) <- c(temp,key_value[[1]][1])
				}
			}
		
			if(is.null(tags1))
				tags1 <- ""
		
			if(bimaps[i,'filter1'] != "")
			{
				filter1 = bimaps[i,'filter1']
			}
		
			if(bimaps[i,'filter2'] != "")
			{
				filter2 = bimaps[i,'filter2']
			}
		
			## All attributes which not set as tagname are added as Rattribnames
			attributes1 <- strsplit(tableinfo[tableinfo[['tablename']] == bimaps[i,'table1'],'fieldnames'],";")[[1]]
			attributes1 <- attributes1[attributes1 != tableinfo[tableinfo[['tablename']] == bimaps[i,'table1'],'mainCol']]
			attributes1 <- attributes1[attributes1 != tags2]
			names(attributes1) <- attributes1
		
			if(bimaps[i,'tagname2'] != "")
			{
				tagnames <- strsplit(bimaps[i,'tagname2'],";")
			
				for(j in seq(along=tagnames[[1]]))
				{
					key_value <- strsplit(tagnames[[1]][j],"=")							
					temp <- names(tags2)
					tags2 <- c(tags2,key_value[[1]][2])
					names(tags2) <- c(temp,key_value[[1]][1])
				}
			}
		
			if(is.null(tags2))
				tags2 <- ""

			## All attributes which not set as tagname are added as Rattribnames
			if(bimaps[i,'table2'] != "")
			{
				attributes2 <- strsplit(tableinfo[tableinfo[['tablename']] == bimaps[i,'table2'],'fieldnames'],";")[[1]]
				attributes2 <- attributes2[attributes2 != tableinfo[tableinfo[['tablename']] == bimaps[i,'table2'],'mainCol']]		
				attributes2 <- attributes2[attributes2 != tags2]		
				names(attributes2) <- attributes2
			}

			if(tags1 == "")
				tags1 <- as.character(NA)
		
			if(tags2 == "")
				tags2 <- as.character(NA)

			#cat(i,"nrow Bimaps: ",nrow(bimaps),"table1: ",bimaps[i,'table1'],"- table2: ",bimaps[i,'table2'],"\n")
			if(bimaps[i,'table2'] != "")
			{
			chain=list(new("L2Rlink",tablename=bimaps[i,'table1'],Lcolname=as.character(tableinfo[tableinfo[1] == bimaps[i,'table1'],'mainCol']),Rcolname="_id"),
					   new("L2Rlink",tablename=bimaps[i,'table2'],Lcolname="_id",Rcolname=as.character(tableinfo[tableinfo[1] == bimaps[i,'table2'],'mainCol'])))
			}
			else
			{
				print(fields[tableinfo[1] == bimaps[i,'table1']][[1]])
				print(class(fields[tableinfo[1] == bimaps[i,'table1']][[1]]))
			chain=list(new("L2Rlink",tablename=bimaps[i,'table1'],Lcolname=as.character(tableinfo[tableinfo[1] == bimaps[i,'table1'],'mainCol']),Rcolname=fields[tableinfo[1] == bimaps[i,'table1']][[1]]))
			}

			# Create new Bimap
			myBimap_T1_2_T2 <- new("AnnDbBimap",L2Rchain=chain,
								objName=bimaps[i,'name'],objTarget="ecoliK12CHIP",
								datacache=datacache,direction=bimaps[i,'revmap'])
								
			#direction(myBimap_T1_2_T2) <- revmap
									
			# Add tagnames
			myBimap_T1_2_T2@L2Rchain[[1]]@tagname <- tags1		

			if(bimaps[i,'table2'] != "")
				myBimap_T1_2_T2@L2Rchain[[2]]@tagname <- tags2
	
			# Add Rattribnames
			myBimap_T1_2_T2@L2Rchain[[1]]@Rattribnames <- attributes1	

			if(bimaps[i,'table2'] != "")	
				myBimap_T1_2_T2@L2Rchain[[2]]@Rattribnames <- attributes2	
		
			# Add Filters
			myBimap_T1_2_T2@L2Rchain[[1]]@filter <- filter1

			if(bimaps[i,'table2'] != "")
				myBimap_T1_2_T2@L2Rchain[[2]]@filter <- filter2
			
			# If revmap true
			#revmap(myBimap_T1_2_T2, objName='test')
			
			assign(bimaps[i,'name'],myBimap_T1_2_T2,envir=env)
		
			## Get MapCounts for Bimap
			filter1 <- gsub("[{]","",filter1)
			filter2 <- gsub("[{]","",filter2)
			filter1 <- gsub("[}]","",filter1)
			filter2 <- gsub("[}]","",filter2)		
		
			#cat(i,' MAPCOUNT ',bimaps[i,'name'],"\n")

			if(bimaps[i,'table2'] != "")
			{
				sql <- paste("SELECT COUNT(DISTINCT",tableinfo[tableinfo$tablename==bimaps[i,'table1'],'mainCol'],") FROM",bimaps[i,'table1'],"a,",bimaps[i,'table2'],"b WHERE a._id = b._id AND",filter1,"AND",filter2)
			}
			else
			{
				sql <- paste("SELECT COUNT(DISTINCT",tableinfo[tableinfo$tablename==bimaps[i,'table1'],'mainCol'],") FROM",bimaps[i,'table1'],"a WHERE",filter1)
			}
		
			MapCount <- as.integer(dbGetQuery(dbconn,sql)[1,1])
			names(MapCount) <- bimaps[i,'name']
			MapCounts <- c(MapCounts,MapCount)
		}
		,error=function(e) 
		{ 
			cat("Cannot create Bimap object '",bimaps[i,'name'],"'.\n") 
		})
	
	}
	
	assign('MAPCOUNTS',MapCounts,envir=env)
	
	# update map counts table. Needed for showQCData function()
	sql <- "DELETE FROM map_counts WHERE map_name != 'TOTAL'"
	dbGetQuery(dbconn,sql)
	
	for(i in seq(along=MapCounts))
	{
		sql <- paste("INSERT INTO map_counts VALUES('",names(MapCounts[i]),"',",MapCounts[i],")",sep="")
		dbGetQuery(dbconn,sql)
	}
		
	return(TRUE)
}
