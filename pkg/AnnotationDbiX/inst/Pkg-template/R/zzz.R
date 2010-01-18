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
    BimapObjs <- .createBimapObj(dbconn,datacache)
    
    for(i in seq(BimapObjs[[1]]))
    {
    	assign(paste('@ANNOBJPREFIX@',BimapObjs[[1]][i],sep=""),BimapObjs[[2]][[i]],envir=datacache)
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
