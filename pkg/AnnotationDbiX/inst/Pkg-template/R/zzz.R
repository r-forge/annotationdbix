require("AnnotationDbiX")
require("RSQLite")

datacache <- new.env(hash=TRUE, parent=emptyenv())

#@ANNOBJPREFIX@_dbInfo <- function() dbInfo(datacache)
#@ANNOBJPREFIX@_dbconn <- function() dbconn(datacache)
#@ANNOBJPREFIX@_dbfile <- function() dbfile(datacache)

.onLoad <- function(libname, pkgname)
{
    ## Connect to the SQLite DB
    dbfile <- system.file("extdata", "@DBFILENEW@", package=pkgname, lib.loc=libname)
    dbconn <- dbConnect(SQLite(),dbname = dbfile)
    dbschema <- function(x=system.file(package=pkgname,lib.loc=libname,'extdata','@DBFILENEW@'), show.indices=FALSE,tableName="") .dbSchema(x, show.indices,tableName)
    qc <- function() showQCData("@ANNOBJPREFIX@", datacache) 
	
	assign("@ANNOBJPREFIX@_dbfile",function() dbfile,envir=datacache)
	assign("dbfile",dbfile,envir=datacache) # for compatibility
    #dbconn <- dbFileConnect(dbfile) ! dbfiledbFileConnect opens db in read-only mode
    assign("@ANNOBJPREFIX@_dbconn",function() dbconn,envir=datacache)
    assign("dbconn",dbconn,envir=datacache) # for compatibility with showQCData()
    assign("@ANNOBJPREFIX@_dbschema", dbschema, envir=datacache)
    assign("@ANNOBJPREFIX@", qc, envir=datacache)
	assign("@ANNOBJPREFIX@ORGANISM","@ORGANISM@",envir=datacache)
	assign("@ANNOBJPREFIX@_dbInfo",function() {dbGetQuery(dbconn,"SELECT * FROM 'metadata'")},envir=datacache)
	
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
