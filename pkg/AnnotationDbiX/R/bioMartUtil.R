setGeneric("getBioMartFromXML", signature = c("x"), function(x,results=FALSE) standardGeneric("getBioMartFromXML"))
setGeneric("getBioMartAttributes", signature = c("x"), function(x) standardGeneric("getBioMartAttributes"))
setGeneric("getBioMartFilters", signature = c("x"), function(x) standardGeneric("getBioMartFilters"))
#setGeneric("getBioMartFromGUI", signature = c("x"), function(x) standardGeneric("getBioMartFromGUI"))

setMethod("getBioMartFromXML", signature("character"), function(x,results=FALSE) 
{
	doc <- xmlInternalTreeParse(x)
	root <- xmlRoot(doc)
	
	bioMart <- unlist(xpathApply(root,"/Query",xmlAttrs))
	dataset <- unlist(xpathApply(root,"/Query/Dataset",xmlAttrs))
	free(doc)
	
	Mart = useMart(bioMart['virtualSchemaName'], dataset = dataset['name'])

	if(results)
	{
		attr <- getBioMartAttributes(x)
		filt <- getBioMartFilters(x)
		
		if(length(filt[[1]]) == 0)
			return(getBM(attributes=attr,mart=Mart))		
		else
			return(getBM(attributes=attr,filters=filt$filters,values=filt$values,mart=Mart))
	}
	else
		return(Mart)
})

setMethod("getBioMartAttributes", signature("character"), function(x) 
{
	doc <- xmlInternalTreeParse(x)
	root <- xmlRoot(doc)
	
	Attributes <- unlist(xpathApply(root,"/Query/Dataset/Attribute",xmlAttrs))
	free(doc)
	
	return(Attributes)
})

setMethod("getBioMartFilters", signature("character"), function(x) 
{	
	doc <- xmlInternalTreeParse(x)
	root <- xmlRoot(doc)
	
	Filters <- xpathApply(root,"/Query/Dataset/Filter",xmlAttrs)	
	free(doc)
	
	res1 <- sapply(Filters,function(x) {x[1]})

	res2 <- lapply(Filters,function(x) 
		{
			if(names(x[2]) == "excluded")
				if(x[2] == 1)
					return(TRUE)
				else
					return(FALSE)
			else
				return(unlist(strsplit(x[2],",")))
		})
	
	return(list(filters=res1,values=res2))
})

#setMethod("getBioMartFromGUI", signature("character"), function(x,results=FALSE) 
getBioMartFromGUI <- function()
{ 

	selectDatabase <- function(panel)
	{
		mart = useDataset(as.character(panel$db),mart=mart)
		
		# Set Filters
		filters = listFilters(mart)
		filters <- cbind(filters,apply(filters,1,function (x){return(paste(x,collapse=" | "))}))
	
		# Add filters Listbox
		rp.listbox(panel, filters, as.vector(filters[,1]), rows=30, labels=as.vector(filters[,3]),title='Set Filters',action=selectFilters, pos = list(row = 0, column = 0,grid = "g1", width="30", height="30"))
		
		rp.listbox(panel, values, as.vector(filters[,1]), rows=30, labels=as.vector(filters[,3]),title='Set Filters',action=selectFilters, pos = list(row = 0, column = 1,grid = "g1", width="30", height="30"))
		
		panel
	}
	
	selectMart <- function(panel)
	{
		mart <- useMart(as.character(panel$biomarts))
	
		## Choose Database
		dbs <- listDatasets(mart)
		dbs <- cbind(dbs,apply(dbs,1,function (x){return(paste(x,collapse=" | "))}))
		
		# Add Database Listbox
		rp.listbox(panel, db, as.vector(dbs[,1]), rows=20, labels=as.vector(dbs[,3]),title='Select Database',action=selectDatabase, pos = list(row = 0, column = 0,grid = "g1", width="60", height="30"))
	
		panel
	}
	
	
	## Choose Mart
	marts <- listMarts()
	
	marts <-  data.frame(cbind(marts,apply(marts,1,function (x)
	{
		return(paste(x,collapse=" | "))
	})))

	panel <- rp.control(title='Biomart Configuration',panelname='biomart')
	rp.grid(panel, "g1", pos = list(row = 0, column = 0))
	#rp.grid(panel, "g2", pos = list(row = 0, column = 1))
	#rp.grid(panel, "g3", pos = list(row = 0, column = 2))

	# Add Mart Listbox
	rp.listbox(panel, biomarts, as.vector(marts[,1]), rows=20, labels=as.vector(marts[,3]),title='Select Mart',action=selectMart, pos = list(row = 0, column = 0,grid = "g1", width="60", height="30"))
	
	
	
	#selMart <- useMart(as.character(selMartStr))
	
	# Choose Database
	#dbs <- listDatasets(selMart)
	#dbs <- cbind(dbs,apply(dbs,1,function (x){return(paste(x,collapse=" | "))}))
	
	#selDbStr <- guiDlgList(sort(as.vector(dbs[,4])),message='Choose Database',title='Database Selection',multi=FALSE,default=1,width=100)
	
	#selDbStr <- dbs[match(selDbStr,dbs[,4]),1]

	#if(length(selDbStr) == 0)
	#{
	#	print('No Database selected!')
	#	return(1)
	#}
	
	#selMart = useDataset(as.character(selDbStr),mart=selMart)
	
	# Set Filters
	#filters = listFilters(selMart)
	#filters <- cbind(filters,apply(filters,1,function (x){return(paste(x,collapse=" | "))}))

	#selFiltersStr <- guiDlgList(sort(as.vector(filters[,3])),message='Set Filters',title='Set Filtes',multi=TRUE,default=1,width=100)
	
	#selFiltersStr <- filters[match(selFiltersStr,filters[,3]),1]


}
