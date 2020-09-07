computeLoads <- function(rawdir, dbName, 
                         dbTable, numcolsFile, 
                         substFile, 
                         detLimOper,
                         outTableZu, outTableAb){

  library(readxl)
  
  setwd(rawdir)
  
  # read excel table, all cells as string
  db <- as.data.frame(readxl::read_excel(dbName, 
                                         sheet=dbTable, 
                                         col_types="text", 
                                         skip=3, 
                                         na=c("na", "nb","NA")),
                      stringsAsFactors=FALSE)

  # read text file with names of numerical columns
  numcolsName <- scan(file=numcolsFile, 
                      what='character', 
                      quiet=TRUE, 
                      fileEncoding = 'utf-8')
  
  # read text file with names of substances
  substName <- scan(file=substFile, 
                      what='character', 
                      quiet=TRUE, 
                      fileEncoding = 'utf-8')
  
  # format date columns
  tCols <- grep(pattern='tBeg|tEnd', x = colnames(db)) # identify columns
  for(col in tCols){ # loop over identified columns
    db[, col] <- as.POSIXct(db[, col],
                            format="%d.%m.%Y %H:%M",
                            tz="Etc/GMT-1")
  }
  
  # format numeric columns
  for(col in numcolsName){
    x <- db[, col] # grab column
    xsplt <- strsplit(x=x, split='<') # split at '<'
    rowsDetLim <- which(sapply(X=xsplt, FUN=length) > 1) # identify items with '<'
    x <- gsub(pattern='<', replacement='', x=x) # remove '<'
    x <- gsub(pattern=',', replacement='.', x=x) # replace comma with point
    x <- as.numeric(x) # convert to number
    x[rowsDetLim] <- x[rowsDetLim]*detLimOper # apply operator to entries under detection limit
    db[, col] <- x # replace original values in db
  }

  # format boolean columns
  for(i in 1:ncol(db)){
    boolCol <- sum(grepl(pattern='TRUE|FALSE', x=db[, i]), na.rm=TRUE)
    if(boolCol > 1){
      db[, i] <- as.logical(db[, i])
    }
  }

  # grab numbers of columns containing substance concentrations
  ccols <- unlist(lapply(X=substName, 
                         FUN=function(a){
                           grep(pattern=a, x=colnames(db))
                         }))
  
  # filter db for columns containing only concentrations 
  cdb <- db[, ccols]
  
  # identify columns containing inflow and outflow concentrations and make corresponding
  # data.frames
  ccolsZu <- grep(pattern='_zu' , x=colnames(cdb))
  ccolsAb <- grep(pattern='_ab' , x=colnames(cdb))
  cdbZu <- cdb[, ccolsZu]
  cdbAb <- cdb[, ccolsAb]
  
  # make matrices to store loads
  loadsZu <- matrix(NA, nrow=nrow(cdbZu), 
                    ncol=ncol(cdbZu),
                    dimnames=list(NULL, colnames(cdbZu)))
  loadsAb <- matrix(NA, nrow=nrow(cdbAb), 
                    ncol=ncol(cdbAb),
                    dimnames=list(NULL, colnames(cdbAb)))
  
  # compute loads as V * c (c = event mean concentration)
  for(col in 1:ncol(loadsZu)){
    loadsZu[, col] <- db$Abflussvol_l_zu * cdbZu[, col]
  }
  
  for(col in 1:ncol(loadsAb)){
    loadsAb[, col] <- db$Abflussvol_l_ab * cdbAb[, col]
  }
  
  # make output tables and write them in rawdir
  loadsZu <- cbind(data.frame(tBegRain=db$tBegRain),
                   loadsZu)
  loadsAb <- cbind(data.frame(tBegRain=db$tBegRain),
                   loadsAb)
  write.table(loadsZu, file=outTableZu, quote=FALSE, row.names=FALSE,
              sep=';')
  write.table(loadsAb, file=outTableAb, quote=FALSE, row.names=FALSE,
              sep=';')
}
