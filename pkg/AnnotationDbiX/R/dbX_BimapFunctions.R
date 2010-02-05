## Create bimap objects dyn. from database
.createBimapObj <- function(dbconn,datacache)
{	
	## Check if bimap_meta exists
	if(!dbExistsTable(dbconn,'bimap_meta'))
	{
		cat('No Bimap Objects available\n')
		return(NULL)
	}
	
	## Get bimap objects
	sql <- "SELECT * FROM bimap_meta"
	bimaps <- dbGetQuery(dbconn,sql)

	## Get tableinfo
	sql <- "SELECT * FROM table_master_meta"
	tableinfo <- dbGetQuery(dbconn,sql)
	mainCol <- apply(tableinfo[2],1,function(x) strsplit(x,";")[[1]][1])
	tableinfo <- as.data.frame(cbind(tableinfo,mainCol),stringsAsFactors=FALSE)

	Bimaps <- c()
	
	for(i in 1 : nrow(bimaps))
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
				temp <- names(tags2)
				tags2 <- c(tags2,key_value[[1]][2])
				names(tags2) <- c(temp,key_value[[1]][1])
			}
		}
		
		if(bimaps[i,'filter1'] != "")
		{
			filter1 = bimaps[i,'filter1']
		}
		
		if(bimaps[i,'filter2'] != "")
		{
			filter2 = bimaps[i,'filter2']
		}
		
		## All attributes which not set as tagname are added as Rattribnames
		attributes1 <- strsplit(tableinfo[tableinfo[['tablename']] == bimaps[i,'table1'],'fieldnames'],";")[[1]][1]
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
		
		## All attributes which not set as tagname are added as Rattribnames
		attributes2 <- strsplit(tableinfo[tableinfo[['tablename']] == bimaps[i,'table2'],'fieldnames'],";")[[1]][1]
		attributes2 <- attributes2[attributes2 != tableinfo[tableinfo[['tablename']] == bimaps[i,'table2'],'mainCol']]
		attributes2 <- attributes2[attributes2 != tags2]
		names(attributes2) <- attributes2
		
		if(is.null(tags1))
			tags1 <- as.character(NA)
		
		if(is.null(tags2))
			tags2 <- as.character(NA)
		
		#cat(i,"nrow Bimaps: ",nrow(bimaps),"table1: ",bimaps[i,'table1'],"- table2: ",bimaps[i,'table2'],"\n")
		
		chain=list(new("L2Rlink",tablename=bimaps[i,'table1'],Lcolname=as.character(tableinfo[tableinfo[1] == bimaps[i,'table1'],'mainCol']),Rcolname="_id"),
				   new("L2Rlink",tablename=bimaps[i,'table2'],Lcolname="_id",Rcolname=as.character(tableinfo[tableinfo[1] == bimaps[i,'table2'],'mainCol'])))
		
		myBimap_T1_2_T2 <- new("AnnDbBimap",L2Rchain=chain,
				objName=bimaps[i,'name'],objTarget="ecoliK12CHIP",
				datacache=datacache)
		
		# Add tagnames
		myBimap_T1_2_T2@L2Rchain[[1]]@tagname <- tags1		
		myBimap_T1_2_T2@L2Rchain[[2]]@tagname <- tags2
		
		# Add Rattribnames
		myBimap_T1_2_T2@L2Rchain[[1]]@Rattribnames <- attributes1		
		myBimap_T1_2_T2@L2Rchain[[2]]@Rattribnames <- attributes2	
		
		# Add Filters
		myBimap_T1_2_T2@L2Rchain[[1]]@filter <- filter1
		myBimap_T1_2_T2@L2Rchain[[2]]@filter <- filter2
		
		Bimaps <- c(Bimaps,myBimap_T1_2_T2)
		
	}
	data <- list(bimaps[['name']],Bimaps)
	
	return(data)
}

.getMapCounts <- function(dbconn,datacache)
{
	## Check if bimap_meta exists
	if(!dbExistsTable(dbconn,'bimap_meta'))
	{
		cat('No Bimap Objects available\n')
		return()
	}
	
	## Get bimap objects
	sql <- "SELECT * FROM bimap_meta"
	bimaps <- dbGetQuery(dbconn,sql)

	## Get tableinfo
	sql <- "SELECT * FROM table_master_meta"
	tableinfo <- dbGetQuery(dbconn,sql)
	mainCol <- apply(tableinfo[2],1,function(x) strsplit(x,";")[[1]][1])
	tableinfo <- as.data.frame(cbind(tableinfo,mainCol),stringsAsFactors=FALSE)
		
	MapCounts <- c()
	## Get MapCounts from all Bimaps

	for(i in seq(along=bimaps[['name']]))
	{
		if(bimaps[i,'filter1'] == "")
			filter1 = 1
		else
			filter1 = bimaps[i,'filter1']
		
		if(bimaps[i,'filter2'] == "")
			filter2 = 1
		else
			filter2 = bimaps[i,'filter2']
			
		filter1 <- gsub("[{]","",filter1)
		filter2 <- gsub("[{]","",filter2)
		filter1 <- gsub("[}]","",filter1)
		filter2 <- gsub("[}]","",filter2)		
		
		sql <- paste("SELECT COUNT(DISTINCT",tableinfo[tableinfo$tablename==bimaps[i,'table1'],'mainCol'],") FROM",bimaps[i,'table1'],"a,",bimaps[i,'table2'],"b WHERE a._id = b._id AND",filter1,"AND",filter2)
		
		MapCount <- as.integer(dbGetQuery(dbconn,sql)[1,1])
		names(MapCount) <- bimaps[i,'name']
		MapCounts <- c(MapCounts,MapCount)
	}
	return(MapCounts)
}
