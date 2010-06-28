setGeneric("getBioMartFromXML", signature = c("x"), function(x) standardGeneric("getBioMartFromXML"))
setGeneric("getBioMartAttributes", signature = c("x"), function(x) standardGeneric("getBioMartAttributes"))
setGeneric("getBioMartFilters", signature = c("x"), function(x) standardGeneric("getBioMartFilters"))

setMethod("getBioMartFromXML", signature("character"), function(x) 
{
	doc <- xmlRoot(xmlInternalTreeParse(x))
	
	bioMart <- unlist(xpathApply(doc,"/Query",xmlAttrs))
	dataset <- unlist(xpathApply(doc,"/Query/Dataset",xmlAttrs))
	
	Mart = useMart(bioMart['virtualSchemaName'], dataset = dataset['name'])

})

setMethod("getBioMartAttributes", signature("character"), function(x) 
{
	doc <- xmlRoot(xmlInternalTreeParse(x))
	
	Attributes <- unlist(xpathApply(doc,"/Query/Dataset/Attribute",xmlAttrs))
})

setMethod("getBioMartFilters", signature("character"), function(x) 
{
	doc <- xmlRoot(xmlInternalTreeParse(x))
	
	Filters <- xpathApply(doc,"/Query/Dataset/Filter",xmlAttrs)	
	
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
