setGeneric("getBioMartFromXML", signature = c("x"), function(x,results=FALSE) standardGeneric("getBioMartFromXML"))
setGeneric("getBioMartAttributes", signature = c("x"), function(x) standardGeneric("getBioMartAttributes"))
setGeneric("getBioMartFilters", signature = c("x"), function(x) standardGeneric("getBioMartFilters"))

setMethod("getBioMartFromXML", signature("character"), function(x,results=FALSE) 
{
	doc <- xmlRoot(xmlInternalTreeParse(x))
	
	bioMart <- unlist(xpathApply(doc,"/Query",xmlAttrs))
	dataset <- unlist(xpathApply(doc,"/Query/Dataset",xmlAttrs))
	free(doc)
	
	Mart = useMart(bioMart['virtualSchemaName'], dataset = dataset['name'])

	if(results)
	{
		attr <- getBioMartAttributes(x)
		filt <- getBioMartFilters(x)
		
		return(getBM(attributes=attr,filters=filt$filters,values=filt$values,mart=Mart))
	}
	else
		return(Mart)
})

setMethod("getBioMartAttributes", signature("character"), function(x) 
{
	doc <- xmlRoot(xmlInternalTreeParse(x))
	
	Attributes <- unlist(xpathApply(doc,"/Query/Dataset/Attribute",xmlAttrs))
	free(doc)
	
	return(Attributes)
})

setMethod("getBioMartFilters", signature("character"), function(x) 
{
	doc <- xmlRoot(xmlInternalTreeParse(x))
	
	Filters <- xpathApply(doc,"/Query/Dataset/Filter",xmlAttrs)	
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
