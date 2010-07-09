library(ecoliK12Test.db)

ecoliK12TestMAPCOUNTS
ls("package:ecoliK12Test.db")
all_probes <- ls(ecoliK12TestENTREZID)
length(all_probes)
ecoliK12TestENTREZID$"ACBT-Ecoli-07:Cons-O-01:A-01"
probes <- sample(all_probes, 5)
probes

syms <- unlist(mget(probes, ecoliK12TestALIAS))
syms

unlist(mget(probes, ecoliK12TestREFSEQ))
unlist(mget(probes, ecoliK12TestENTREZID))

zz <- as.list(ecoliK12TestALIAS)

unlist(mget(syms, revmap(ecoliK12TestALIAS)))

as.list(revmap(ecoliK12TestPATH)[1])

x <- ecoliK12TestPATH
revx <- ecoliK12TestPATH2PROBE
revx2 <- revmap(x, objName="PATH2PROBE")
revx2
identical(revx, revx2)

as.list(revx[1])

toTable(ecoliK12TestGO_BP_ALL[probes])
toTable(x)[1:6,]

"%w/o%" <- function(x,y) x[!x %in% y] #--  x without y

#####################################################3
library(Biobase)

exprsFile <- "../../data/Theresa/validator/exprsData.txt"
exprs <- as.matrix(read.table(exprsFile, header=TRUE, sep="\t",
                              row.names=1,
                              as.is=TRUE))

minimalSet <- new("ExpressionSet", exprs=exprs)
pDataFile <- "../../data/Theresa/validator/pData.txt"
pData <- read.table(pDataFile,
                    row.names=1, header=TRUE, sep="\t")
metadata <- data.frame(labelDescription=
                       c("Time point", 
                         "fermentation status in relation to induction", "Technical Replicate"),
                       row.names=c("time", "type", "replicate"))
phenoData <- new("AnnotatedDataFrame", 
                 data=pData, varMetadata=metadata)
phenoData

annotation = "ecoliK12Test"

experimentData <- new("MIAME",
  name="Gerald Striedner",
  lab="MoFern Boku",
  contact="gerald.striedner@boku.ac.at",
  title="K12 New Array Test Data",
  abstract="An example ExpressionSet",
  url="www.lab.not.exist",
  other=list(
    notes="Created from text files"
  ))

exampleSet <- new("ExpressionSet", 
                  exprs=exprs, 
                  phenoData=phenoData, 
                  experimentData=experimentData,
                  annotation="ecoliK12Test")


entrezIds <- mget(featureNames(exampleSet), envir=ecoliK12TestENTREZID)
haveEntrezId <- names(entrezIds[!is.na(entrezIds)])
#haveEntrezId <- names(entrezIds)[sapply(entrezIds, function(x) !is.na(x))]
numNoEntrezId <- length(featureNames(exampleSet)) - length(haveEntrezId)
exampleSet <- exampleSet[haveEntrezId, ]

haveGo <- sapply(mget(featureNames(exampleSet), ecoliK12TestGO_BP_ALL),
                 function(x) {
                     if (length(x) == 1 && is.na(x)) 
                       FALSE 
                     else TRUE
                 })
numNoGO <- sum(!haveGo)
exampleSet <- exampleSet[haveGo, ]

IqrCutoff <- 0.2
exampleSetIqr <- apply(exprs(exampleSet), 1, IQR)
selected <- exampleSetIqr > IqrCutoff

nsFiltered <- exampleSet[selected, ]

library(genefilter)
library(annotate)

numNsWithDups <- length(featureNames(nsFiltered))
nsFilteredIqr <- exampleSetIqr[selected]
uniqGenes <- findLargest(featureNames(nsFiltered), nsFilteredIqr, "ecoliK12Test")
nsFiltered <- nsFiltered[uniqGenes, ]
numSelected <- length(featureNames(nsFiltered))

##set up some colors
library(RColorBrewer)
library(GOstats)

BCRcols = ifelse(nsFiltered$type == "before", "goldenrod", "skyblue")
cols = brewer.pal(10, "RdBu")

coliUniverse <- featureNames(nsFiltered)

temp <- unlist(mget(coliUniverse, ecoliK12TestENTREZID))
entrezUniverse <- temp[!is.na(temp)]

entrezUniverse <- entrezUniverse[!duplicated(entrezUniverse)]

if (any(duplicated(entrezUniverse)))
  stop("error in gene universe: cant have duplicate Entrez Gene Ids")

## Also define an alternate universe based on the entire chip
chipColiUniverse <- featureNames(exampleSet)
chipEntrezUniverse <- mget(chipColiUniverse, ecoliK12TestENTREZID)
chipEntrezUniverse <- unique(unlist(chipEntrezUniverse)) 

ttestCutoff <- 0.05
ttests = rowttests(nsFiltered, "type")

smPV = ttests$p.value < ttestCutoff

pvalFiltered <- nsFiltered[smPV, ]
selectedEntrezIds <- unlist(mget(featureNames(pvalFiltered),
                                 ecoliK12TestENTREZID))
selectedSymbols <- unlist(mget(featureNames(pvalFiltered),
                                 ecoliK12TestSYMBOL))

hgCutoff <- 0.05
params <- new("GOHyperGParams",
              geneIds=selectedEntrezIds,
              universeGeneIds=entrezUniverse,
              annotation="ecoliK12Test.db",
              ontology="BP",
              pvalueCutoff=hgCutoff,
              conditional=FALSE,
              testDirection="over")
paramsCond <- params
conditional(paramsCond) <- TRUE

hgOver <- hyperGTest(params)
hgCondOver <- hyperGTest(paramsCond)

df <- summary(hgOver)
names(df)                               # the columns
dim(summary(hgOver, pvalue=0.1))
dim(summary(hgOver, categorySize=10))
pvalues(hgOver)[1:3]

oddsRatios(hgOver)[1:3]

expectedCounts(hgOver)[1:3]

geneCounts(hgOver)[1:3]
universeCounts(hgOver)[1:3]

length(geneIds(hgOver))
length(geneIdUniverse(hgOver)[[3]])
goDag(hgOver)

geneMappedCount(hgOver)
universeMappedCount(hgOver)

conditional(hgOver)
testDirection(hgOver)
testName(hgOver)

htmlReport(hgCondOver, file="K12neu_hgco.html")

sigCategories <- function(res, p) {
    if (missing(p))
      p <- pvalueCutoff(res)
    pv <- pvalues(res)
    goIds <- names(pv[pv < p])
    goIds
}

featureNames(exampleSet)[1:10] -> featnam
ids <- getPMID(featnam,"ecoliK12Test")
ids = unlist(ids, use.name=F)
ids = unique(ids[!is.na(as.numeric(ids))])

x <- pubmed(ids)
a <- xmlRoot(x)
numAbst <- length(xmlChildren(a))

arts <- vector("list", length(numAbst))
absts <- rep(NA, numAbst)
for(i in 1:numAbst) {
   arts[[i]] <- buildPubMedAbst(a[[i]])
   absts[i] <- abstText(arts[[i]])
}
arts[[7]]

found <- grep("protein", absts)
goodAbsts <- arts[found]
length(goodAbsts)

y <- genbank(ids[1:10], type="uid")
b <- xmlRoot(x)

fname <- "pm1"
pmAbst2HTML(goodAbsts, filename = fname)
fnameBase <- "pm2"
pmAbst2HTML(goodAbsts, filename=fnameBase, frames=T)

