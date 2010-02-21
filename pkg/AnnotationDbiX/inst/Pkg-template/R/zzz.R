require("AnnotationDbiX")
require("RSQLite")

datacache <- new.env(hash=TRUE, parent=emptyenv())

#@ANNOBJPREFIX@ <- function() showQCData("@ANNOBJPREFIX@", datacache)
#@ANNOBJPREFIX@_dbInfo <- function() dbInfo(datacache)
@ANNOBJPREFIX@_dbconn <- function() dbconn(datacache)
@ANNOBJPREFIX@_dbfile <- function() dbfile(datacache)
@ANNOBJPREFIX@ORGANISM <- "@ORGANISM@"

.onLoad <- function(libname, pkgname)
{
    ## Connect to the SQLite DB
    dbfile <- system.file("extdata", "@DBFILENEW@", package=pkgname, lib.loc=libname)
    assign("dbfile",dbfile,envir=datacache)
    dbconn <- dbFileConnect(dbfile)
    assign("dbconn",dbconn,envir=datacache)
    
    ## Assigned _dbschema function to datacache  
    dbschema <- function(x=system.file(package=pkgname,lib.loc=libname,'extdata','@DBFILENEW@'), show.indices=FALSE,tableName="") .dbSchema(x, show.indices,tableName)
    assign("@ANNOBJPREFIX@_dbschema", dbschema, envir=datacache)
    #assign("@ANNOBJPREFIX@_dbconn",function() dbconn(datacache), envir=datacache) ??? Problems with export
	#assign("@ANNOBJPREFIX@_dbfile",function() dbfile(datacache), envir=datacache)
	#assign("@ANNOBJPREFIX@ORGANISM","@ORGANISM@", envir=datacache)
	
    ## Create the AnnObj instances 
    newBimap <- new.env(hash=TRUE,parent=emptyenv()) 
    .createBimapObjs(dbconn,datacache,newBimap)
       
    ## Add prefix to bimap names
    for(i in seq(ls(newBimap)))
    {
    	bimapName <- ls(newBimap)[i]
    	assign(paste('@ANNOBJPREFIX@',bimapName,sep=""),get(bimapName,newBimap),envir=datacache)
    }
    		  
    keys <- ls(datacache, all.names=TRUE)
    
    ## Export all objects to the package namespace
    for (key in keys)
	{
    	ns <- asNamespace(pkgname)
    	assign(key,datacache[[key]], envir=ns)
    	
    	namespaceExport(ns, key)
	}    	
}

.onUnload <- function(libpath)
{
    dbFileDisconnect(@ANNOBJPREFIX@_dbconn())
}
