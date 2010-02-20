require("AnnotationDbiX")
require("RSQLite")

datacache <- new.env(hash=TRUE, parent=emptyenv())

#@ANNOBJPREFIX@ <- function() showQCData("@ANNOBJPREFIX@", datacache)
@ANNOBJPREFIX@_dbconn <- function() dbconn(datacache)
@ANNOBJPREFIX@_dbfile <- function() dbfile(datacache)
#@ANNOBJPREFIX@_dbschema <- function(file="", show.indices=FALSE) dbschema(datacache, file=file, show.indices=show.indices)
#@ANNOBJPREFIX@_dbInfo <- function() dbInfo(datacache)

@ANNOBJPREFIX@ORGANISM <- "@ORGANISM@"

.onLoad <- function(libname, pkgname)
{
    #require("methods", quietly=TRUE)
    ## Connect to the SQLite DB
    dbfile <- system.file("extdata", "@DBFILENEW@", package=pkgname, lib.loc=libname)
    assign("dbfile", dbfile, envir=datacache)
    dbconn <- dbFileConnect(dbfile)
    ## Assigned dbconn in datacache
    assign("dbconn", dbconn, envir=datacache)
    
    ## Create the AnnObj instances 
    newBimap <- new.env(hash=TRUE,parent=emptyenv()) 
    .createBimapObjs(dbconn,datacache,newBimap)
    
    for(i in seq(ls(newBimap)))
    {
    	bimapName <- ls(newBimap)[i]
    	assign(paste('@ANNOBJPREFIX@',bimapName,sep=""),get(bimapName,newBimap),envir=datacache)
    }
    		  
    keys <- ls(datacache, all.names=TRUE)
  
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
