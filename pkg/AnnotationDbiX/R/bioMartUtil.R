setGeneric("getBioMartFromXML", signature = c("x"), function(x,results=FALSE) standardGeneric("getBioMartFromXML"))
setGeneric("getBioMartAttributes", signature = c("x"), function(x) standardGeneric("getBioMartAttributes"))
setGeneric("getBioMartFilters", signature = c("x"), function(x) standardGeneric("getBioMartFilters"))

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
